;;; diagram-viewer-viewer.el --- Nonfile diagram image viewer -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: diagram-viewer contributors

;;; Commentary:

;; This lazy Core helper owns viewer source markers and checked image bytes.
;; State screens deliberately do not invoke the file-oriented `image-mode'
;; entry point; successful images use its native display and transformations.

;;; Code:

(require 'diagram-viewer)
(require 'image-mode)

(defcustom diagram-viewer-reuse-window t
  "Reuse a visible diagram viewer window for another diagram."
  :type 'boolean
  :group 'diagram-viewer)

(defvar-local diagram-viewer--viewer-source nil
  "Source descriptor owned exclusively by this viewer.")
(defvar-local diagram-viewer--viewer-snapshot nil
  "Property-free source range text at the latest submission.")
(defvar-local diagram-viewer--viewer-request nil
  "Latest request belonging to this viewer.")
(defvar-local diagram-viewer--viewer-generation 0
  "Generation invalidating callbacks from previous submissions.")
(defvar-local diagram-viewer--viewer-transaction nil
  "Unique token of the current submission transaction.")
(defvar-local diagram-viewer--viewer-closing nil
  "Non-nil while this mode is being killed or changed.")
(defvar-local diagram-viewer--viewer-bytes nil
  "Independent checked bytes of the currently displayed image.")
(defvar-local diagram-viewer--viewer-cache-key nil
  "Cache key of the currently displayed image.")
(defvar-local diagram-viewer--viewer-state 'idle
  "Current viewer state: idle, rendering, succeeded, failed or stale.")
(defvar-local diagram-viewer--viewer-renderer nil
  "Explicit renderer ID retained for subsequent submissions.")
(defvar-local diagram-viewer--viewer-error nil
  "Structured diagnostic for the latest failure.")

(defvar diagram-viewer-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map image-mode-map)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'diagram-viewer-rerender)
    (define-key map (kbd "o") #'diagram-viewer-visit-source)
    (define-key map (kbd "s") #'diagram-viewer-save-as)
    (define-key map (kbd "+") #'diagram-viewer-scale-increase)
    (define-key map (kbd "-") #'diagram-viewer-scale-decrease)
    (define-key map (kbd "0") #'diagram-viewer-scale-reset)
    (define-key map (kbd "f") #'diagram-viewer-fit-to-window)
    map)
  "Keymap for `diagram-viewer-mode'.")

(defvar diagram-viewer-mode-hook nil
  "Hook run when entering `diagram-viewer-mode'.")

;; Like image-mode itself this is a hand-written major mode.  Calling the
;; parent entry point would reject TTY state screens before setting up a mode.
(put 'diagram-viewer-mode 'derived-mode-parent 'image-mode)
(defun diagram-viewer-mode ()
  "View checked diagrams in a read-only, nonfile Image-derived buffer.
Use g to refresh, o to visit the source, s to save and q to quit.
Image transformations are available only while displaying an image."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'diagram-viewer-mode mode-name "Diagram Viewer"
        buffer-read-only t buffer-file-name nil buffer-undo-list t)
  (use-local-map diagram-viewer-mode-map)
  (setq-local revert-buffer-function (lambda (&rest _) (diagram-viewer-rerender)))
  (add-hook 'kill-buffer-hook #'diagram-viewer--viewer-cleanup nil t)
  (add-hook 'change-major-mode-hook #'diagram-viewer--viewer-cleanup nil t)
  (run-mode-hooks 'image-mode-hook 'diagram-viewer-mode-hook))

(defun diagram-viewer--viewer-cleanup ()
  "Invalidate callbacks, cancel this viewer's request and detach its markers."
  (let ((request diagram-viewer--viewer-request)
        (source diagram-viewer--viewer-source))
    (cl-incf diagram-viewer--viewer-generation)
    (setq diagram-viewer--viewer-closing t diagram-viewer--viewer-transaction nil
          diagram-viewer--viewer-source nil diagram-viewer--viewer-request nil
          diagram-viewer--viewer-bytes nil diagram-viewer--viewer-cache-key nil)
    (diagram-viewer--release-source source)
    (when request (diagram-viewer-cancel request))))

(defun diagram-viewer--viewer-transaction-current-p (buffer token)
  "Return non-nil if BUFFER still owns the live transaction TOKEN."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (eq major-mode 'diagram-viewer-mode)
              (not diagram-viewer--viewer-closing)
              (eq diagram-viewer--viewer-transaction token)))))

(defun diagram-viewer--viewer-live-source-p (source)
  "Return non-nil when SOURCE still identifies a nonempty live range."
  (condition-case nil
      (let ((buffer (diagram-viewer-source-buffer source))
            (range (diagram-viewer-source-range source)))
        (and (buffer-live-p buffer)
             (eq (marker-buffer (car range)) buffer)
             (eq (marker-buffer (cdr range)) buffer)
             (< (marker-position (car range)) (marker-position (cdr range)))))
    (error nil)))

(defun diagram-viewer--viewer-matches-p (source)
  "Return non-nil if this viewer owns the same live range as SOURCE."
  (let ((owned diagram-viewer--viewer-source))
    (and (eq major-mode 'diagram-viewer-mode)
         (diagram-viewer--viewer-live-source-p owned)
         (diagram-viewer--viewer-live-source-p source)
         (eq (diagram-viewer-source-buffer owned) (diagram-viewer-source-buffer source))
         (eq (diagram-viewer-source-origin-adapter-id owned)
             (diagram-viewer-source-origin-adapter-id source))
         (= (car (diagram-viewer-source-range owned))
            (car (diagram-viewer-source-range source)))
         (= (cdr (diagram-viewer-source-range owned))
            (cdr (diagram-viewer-source-range source))))))

(defun diagram-viewer--viewer-pop (buffer)
  "Display BUFFER, optionally reusing a visible diagram window."
  (let ((window (and diagram-viewer-reuse-window
                     (cl-find-if
                      (lambda (candidate)
                        (with-current-buffer (window-buffer candidate)
                          (eq major-mode 'diagram-viewer-mode)))
                      (window-list nil 'nomini)))))
    (if window
        (progn (set-window-buffer window buffer) (select-window window))
      (pop-to-buffer buffer))))

(defun diagram-viewer--viewer-screen (state message &optional error-data)
  "Show a STATE screen containing MESSAGE and optional ERROR-DATA."
  (setq diagram-viewer--viewer-state state
        diagram-viewer--viewer-error error-data
        diagram-viewer--viewer-bytes nil diagram-viewer--viewer-cache-key nil)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (set-buffer-multibyte t)
    (insert "Diagram Viewer\n\n"
            (if (eq state 'failed)
                (truncate-string-to-width (car (split-string message "[\n\r]")) 200 nil nil "…")
              message)
            "\n\ng: render current source   o: visit source   q: quit\n")
    (when error-data
      (insert (format "\nDiagnostic: %s\nM-x diagram-viewer-show-log for details (stdout/stderr).\n"
                      (or (plist-get error-data :id) "latest"))))
    (goto-char (point-min))
    (setq cursor-type t)
    (set-buffer-modified-p nil)))

(defun diagram-viewer--viewer-display (bytes key)
  "Display checked BYTES using native Image mode and retain KEY."
  (unless (and (stringp bytes) (not (multibyte-string-p bytes))
               (> (length bytes) 0))
    (signal 'diagram-viewer-invalid-output '("Missing checked image bytes")))
  (unless (and (display-images-p) (image-type-available-p 'svg))
    (user-error "This display does not support SVG images"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (set-buffer-multibyte nil)
    (insert bytes)
    (goto-char (point-min))
    (setq-local buffer-file-coding-system 'no-conversion)
    (setq-local image-transform-resize 'fit-window)
    (setq-local image-transform-rotation 0.0)
    (image-mode-setup-winprops)
    (image-toggle-display-image)
    (unless (image-get-display-property)
      (signal 'diagram-viewer-invalid-output '("Native image display failed")))
    (setq diagram-viewer--viewer-bytes (copy-sequence bytes)
          diagram-viewer--viewer-cache-key key
          diagram-viewer--viewer-state 'succeeded
          diagram-viewer--viewer-error nil)
    (set-buffer-modified-p nil)))

(defun diagram-viewer--viewer-receive (buffer generation result)
  "Deliver RESULT to BUFFER only when GENERATION and request identity match."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (eq major-mode 'diagram-viewer-mode)
                 (= generation diagram-viewer--viewer-generation)
                 diagram-viewer--viewer-request
                 (eql (diagram-viewer-result-request-id result)
                      (diagram-viewer-request-id diagram-viewer--viewer-request)))
        (let* ((request diagram-viewer--viewer-request)
               (token diagram-viewer--viewer-transaction)
               (owner diagram-viewer--viewer-source)
               (owner-snapshot diagram-viewer--viewer-snapshot)
               (snapshot (diagram-viewer-request-source-snapshot request))
               (current (and (not (diagram-viewer-result-stale-p result))
                             (diagram-viewer--source-current-p (car snapshot) (cdr snapshot))
                             (diagram-viewer--source-current-p owner owner-snapshot))))
          (when (diagram-viewer--viewer-transaction-current-p buffer token)
            (with-current-buffer buffer
              (cond
               ((not current)
                (diagram-viewer--viewer-screen
                 'stale "Source changed or disappeared; image update suppressed.  Use g to refresh."))
               ((eq (diagram-viewer-result-state result) 'succeeded)
                (condition-case err
                    (diagram-viewer--viewer-display
                     (diagram-viewer-result-artifact-data result)
                     (diagram-viewer-result-cache-key result))
                  (error (diagram-viewer--viewer-screen
                          'failed (error-message-string err) (diagram-viewer--diagnose err)))))
               (t
                (let ((data (diagram-viewer-result-error result)))
                  (diagram-viewer--viewer-screen
                   'failed (or (plist-get data :message) "Rendering was cancelled or failed.") data)))))))))))

(defun diagram-viewer--viewer-start (source renderer bypass-cache)
  "Replace this viewer's source with SOURCE and submit using RENDERER.
BYPASS-CACHE ignores saved artifacts.  SOURCE remains owned by its caller."
  (unless (and (eq major-mode 'diagram-viewer-mode)
               (not diagram-viewer--viewer-closing))
    (user-error "The diagram viewer is no longer active"))
  (let* ((buffer (current-buffer))
         (token (make-symbol "viewer-transaction"))
         (owned (diagram-viewer--copy-source source))
         (snapshot (diagram-viewer--range-text owned))
         (previous-source diagram-viewer--viewer-source)
         (previous-request diagram-viewer--viewer-request)
         (generation (cl-incf diagram-viewer--viewer-generation)))
    ;; Publish ownership before any renderer-controlled cancellation or submit
    ;; callback can reenter.  Such a callback may replace or kill this viewer.
    (setq diagram-viewer--viewer-transaction token
          diagram-viewer--viewer-source owned
          diagram-viewer--viewer-snapshot snapshot
          diagram-viewer--viewer-renderer renderer
          diagram-viewer--viewer-request nil)
    (diagram-viewer--release-source previous-source)
    (diagram-viewer--viewer-screen 'rendering "Rendering diagram…")
    (condition-case err
        (progn
          (when previous-request (diagram-viewer-cancel previous-request))
          (when (diagram-viewer--viewer-transaction-current-p buffer token)
            (let ((request
                   (with-current-buffer buffer
                     (diagram-viewer-submit
                      owned :renderer renderer :bypass-cache bypass-cache
                      :callback (lambda (result)
                                  (when (diagram-viewer--viewer-transaction-current-p buffer token)
                                    (diagram-viewer--viewer-receive buffer generation result)))))))
              (if (diagram-viewer--viewer-transaction-current-p buffer token)
                  (with-current-buffer buffer
                    (setq diagram-viewer--viewer-request request))
                ;; Even a kill during available-p happens before submit returns
                ;; the request.  Its eventual owner must cancel it here.
                (diagram-viewer-cancel request)))))
      (error
       (when (diagram-viewer--viewer-transaction-current-p buffer token)
         (with-current-buffer buffer
           (diagram-viewer--viewer-screen
            'failed (error-message-string err) (diagram-viewer--diagnose err))))
       (signal (car err) (cdr err))))))

(cl-defun diagram-viewer--view (source &key renderer bypass-cache)
  "Display SOURCE with optional RENDERER and BYPASS-CACHE; return its viewer."
  (diagram-viewer--check-source source)
  (let ((buffer (cl-find-if
                 (lambda (candidate)
                   (with-current-buffer candidate (diagram-viewer--viewer-matches-p source)))
                 (buffer-list))))
    (unless buffer
      (setq buffer (generate-new-buffer "*Diagram Viewer*"))
      (with-current-buffer buffer (diagram-viewer-mode)))
    (diagram-viewer--viewer-pop buffer)
    (with-current-buffer buffer
      (diagram-viewer--viewer-start source renderer bypass-cache))
    buffer))

(defun diagram-viewer-rerender ()
  "Re-extract this viewer's source and render it without saved cache.
Without a refresh function, reuse the snapshot only while it is current."
  (interactive)
  (let* ((buffer (current-buffer))
         (token diagram-viewer--viewer-transaction)
         (source diagram-viewer--viewer-source)
         (renderer diagram-viewer--viewer-renderer)
         (snapshot diagram-viewer--viewer-snapshot)
         (refresh (and source (diagram-viewer-source-refresh-function source))))
    (unless (diagram-viewer--viewer-live-source-p source)
      (signal 'diagram-viewer-source-not-found '("Source buffer or range disappeared")))
    (if refresh
        (let ((fresh (funcall refresh source)))
          ;; Refresh returns a newly owned descriptor; do not release the viewer
          ;; itself if a third-party callback returns its input descriptor.
          (unwind-protect
              (when (diagram-viewer--viewer-transaction-current-p buffer token)
                (unless (and (diagram-viewer-source-p fresh)
                             (diagram-viewer--viewer-live-source-p fresh))
                  (signal 'diagram-viewer-source-not-found '("Source refresh found no target")))
                (with-current-buffer buffer
                  (diagram-viewer--viewer-start fresh renderer t)))
            (unless (eq fresh source) (diagram-viewer--release-source fresh))))
      (let ((current (diagram-viewer--source-current-p source snapshot)))
        (when (diagram-viewer--viewer-transaction-current-p buffer token)
          (unless current
            (user-error "Source changed; its adapter cannot refresh the saved snapshot"))
          (with-current-buffer buffer
            (diagram-viewer--viewer-start source renderer t)))))))

(defun diagram-viewer-visit-source ()
  "Visit the live source buffer at this viewer's start marker."
  (interactive)
  (let ((source diagram-viewer--viewer-source))
    (unless (diagram-viewer--viewer-live-source-p source)
      (signal 'diagram-viewer-source-not-found '("Source buffer or range disappeared")))
    (let ((buffer (diagram-viewer-source-buffer source))
          (position (marker-position (car (diagram-viewer-source-range source)))))
      (pop-to-buffer buffer)
      (widen)
      (goto-char position))))

(defun diagram-viewer--viewer-require-image ()
  "Signal a helpful error unless this viewer currently displays an image."
  (unless (and (eq diagram-viewer--viewer-state 'succeeded)
               diagram-viewer--viewer-bytes (image-get-display-property))
    (user-error "No diagram image is displayed; wait for rendering or use g")))

(defun diagram-viewer-save-as (file)
  "Save the currently displayed checked bytes to FILE, confirming overwrite."
  (interactive (progn
                 (diagram-viewer--viewer-require-image)
                 (list (read-file-name "Save diagram as: " nil nil nil "diagram.svg"))))
  (diagram-viewer--viewer-require-image)
  (when (and (or (file-exists-p file) (file-symlink-p file))
             (not (yes-or-no-p (format "Overwrite %s? " file))))
    (user-error "Save cancelled"))
  (let ((bytes diagram-viewer--viewer-bytes)
        (coding-system-for-write 'no-conversion))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert bytes)
      (write-region (point-min) (point-max) file nil 'silent)))
  (message "Saved diagram to %s" file))

(defun diagram-viewer-scale-increase ()
  "Increase the displayed image size with the native Image transformation."
  (interactive)
  (diagram-viewer--viewer-require-image)
  (image-increase-size))

(defun diagram-viewer-scale-decrease ()
  "Decrease the displayed image size with the native Image transformation."
  (interactive)
  (diagram-viewer--viewer-require-image)
  (image-decrease-size))

(defun diagram-viewer-scale-reset ()
  "Reset the displayed image to its native size and rotation."
  (interactive)
  (diagram-viewer--viewer-require-image)
  (image-transform-reset-to-original))

(defun diagram-viewer-fit-to-window ()
  "Fit the displayed image to the current graphical window."
  (interactive)
  (diagram-viewer--viewer-require-image)
  (unless (display-graphic-p)
    (user-error "Fitting an image requires a graphical window"))
  (image-transform-fit-to-window))

(defun diagram-viewer--show-log (&optional diagnostic-id)
  "Show DIAGNOSTIC-ID or the latest viewer diagnostic, including stream tails."
  (let* ((id (or diagnostic-id (plist-get diagram-viewer--viewer-error :id)))
         (data (if id (cl-find id diagram-viewer--diagnostics
                               :key (lambda (entry) (plist-get entry :id)) :test #'equal)
                 (car diagram-viewer--diagnostics)))
         (buffer (get-buffer-create "*Diagram Viewer Log*")))
    (unless data (user-error "No retained diagram diagnostic%s" (if id (format " %s" id) "")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Diagram Viewer diagnostic\n\nReview logs before sharing: renderer output may contain source text.\n\n")
        (dolist (entry (if (and (not id) (not (plist-get data :id)))
                           diagram-viewer--diagnostics (list data)))
          (insert (format "ID: %s\nCondition: %s\n%s\n"
                          (or (plist-get entry :id) "legacy")
                          (plist-get entry :condition) (or (plist-get entry :message) "")))
          (dolist (stream '(:stdout :stderr))
            (when-let* ((tail (plist-get entry stream)))
              (insert (format "\n%s:\n%s\n" stream tail))))
          (insert "\n"))
        (goto-char (point-min))
        (special-mode)))
    (pop-to-buffer buffer)
    buffer))

(provide 'diagram-viewer-viewer)
;;; diagram-viewer-viewer.el ends here
