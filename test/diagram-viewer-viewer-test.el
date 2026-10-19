;;; diagram-viewer-viewer-test.el --- Viewer contract tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Deterministic tests stub submission and native display, never decoding
;; untrusted SVG.  The graphical test uses only Core-validated fixture bytes.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'diagram-viewer-viewer)

(declare-function diagram-viewer-svg-validate-file "diagram-viewer-svg" (path))

(defun diagram-viewer-viewer-test--source ()
  "Return an owned source describing the current buffer."
  (diagram-viewer-source-create
   :text (buffer-substring-no-properties (point-min) (point-max))
   :language 'mermaid :adapter-id 'viewer-test :buffer (current-buffer)
   :range (cons (copy-marker (point-min) nil) (copy-marker (point-max) t))))

(defmacro diagram-viewer-viewer-test--with-viewer (&rest body)
  "Run BODY with isolated source, viewer, submissions and native display."
  (declare (indent 0) (debug t))
  `(save-window-excursion
     (let ((requests nil) (cancelled nil) (next-id 0)
           (diagram-viewer--diagnostics nil)
           (source-buffer (generate-new-buffer " *viewer source*"))
           source viewer)
       (cl-letf (((symbol-function 'diagram-viewer-submit)
                  (cl-function
                   (lambda (input &key renderer callback bypass-cache)
                     (let* ((owned (diagram-viewer--copy-source input))
                            (request (diagram-viewer-request-create
                                      :id (cl-incf next-id) :renderer renderer
                                      :source (diagram-viewer-source-text owned)
                                      :source-snapshot (cons owned (diagram-viewer--range-text owned))
                                      :callback callback :bypass-cache bypass-cache)))
                       (push request requests)
                       request))))
                 ((symbol-function 'diagram-viewer-cancel)
                  (lambda (request)
                    (push request cancelled)
                    (setf (diagram-viewer-request-state request) 'cancelled)))
                 ((symbol-function 'display-images-p) (lambda (&rest _) t))
                 ((symbol-function 'image-type-available-p) (lambda (_) t))
                 ((symbol-function 'image-mode-setup-winprops) #'ignore)
                 ((symbol-function 'image-toggle-display-image)
                  (lambda ()
                    (let ((inhibit-read-only t))
                      (put-text-property (point-min) (point-max) 'display
                                         '(image :type svg :data "stub"))))))
         (unwind-protect
             (progn
               (with-current-buffer source-buffer
                 (insert "graph TD\nA-->B\n")
                 (setq source (diagram-viewer-viewer-test--source)))
               (setq viewer (diagram-viewer--view source :renderer 'fake))
               (with-current-buffer viewer ,@body))
           (dolist (buffer (buffer-list))
             (when (and (buffer-live-p buffer)
                        (with-current-buffer buffer
                          (and (eq major-mode 'diagram-viewer-mode)
                               (eq (and diagram-viewer--viewer-source
                                        (diagram-viewer-source-buffer diagram-viewer--viewer-source))
                                   source-buffer))))
               (kill-buffer buffer)))
           (when (buffer-live-p viewer) (kill-buffer viewer))
           (when (buffer-live-p source-buffer) (kill-buffer source-buffer))
           (diagram-viewer--release-source source)
           (dolist (request requests)
             (diagram-viewer--release-source
              (car (diagram-viewer-request-source-snapshot request)))))))))

(defun diagram-viewer-viewer-test--deliver (request &optional state stale error-data)
  "Deliver a stub result to REQUEST with STATE, STALE and ERROR-DATA."
  (let ((result (diagram-viewer-result-create
                 :request-id (diagram-viewer-request-id request)
                 :state (or state 'succeeded) :stale-p stale :error error-data
                 :artifact "/nonexistent/cache.svg" :artifact-data (unibyte-string 60 115 118 103 47 62)
                 :cache-key (make-string 64 ?a))))
    (funcall (diagram-viewer-request-callback request) result)
    (diagram-viewer--release-source (car (diagram-viewer-request-source-snapshot request)))
    result))

(ert-deftest diagram-viewer-viewer-mode-and-rendering-screen ()
  "The nonfile read-only mode derives from Image and has a rendering screen."
  (diagram-viewer-viewer-test--with-viewer
    (should (derived-mode-p 'image-mode))
    (should buffer-read-only)
    (should-not buffer-file-name)
    (should-not (buffer-modified-p))
    (should (eq diagram-viewer--viewer-state 'rendering))
    (should (string-match-p "Rendering" (buffer-string)))))

(ert-deftest diagram-viewer-viewer-quit-keeps-request-alive ()
  "Quitting hides the viewer without killing it or cancelling its request."
  (diagram-viewer-viewer-test--with-viewer
    (quit-window)
    (should (buffer-live-p viewer))
    (should-not cancelled)
    (should (eq (diagram-viewer-request-state (car requests)) 'pending))
    (diagram-viewer-viewer-test--deliver (car requests))
    (with-current-buffer viewer
      (should (eq diagram-viewer--viewer-state 'succeeded)))))

(ert-deftest diagram-viewer-viewer-request-source-current-checked ()
  "Detached request-owned markers suppress success despite a live viewer source."
  (diagram-viewer-viewer-test--with-viewer
    (diagram-viewer--release-source
     (car (diagram-viewer-request-source-snapshot (car requests))))
    (should (diagram-viewer--viewer-live-source-p diagram-viewer--viewer-source))
    (diagram-viewer-viewer-test--deliver (car requests))
    (should (eq diagram-viewer--viewer-state 'stale))
    (should-not diagram-viewer--viewer-bytes)))

(ert-deftest diagram-viewer-viewer-independent-source-ownership ()
  "Producer, viewer and request markers are three independent owners."
  (diagram-viewer-viewer-test--with-viewer
    (let* ((owned diagram-viewer--viewer-source)
           (requested (car (diagram-viewer-request-source-snapshot (car requests)))))
      (should-not (eq owned source))
      (should-not (eq owned requested))
      (should-not (eq (car (diagram-viewer-source-range source))
                      (car (diagram-viewer-source-range owned))))
      (diagram-viewer--release-source source)
      (should (diagram-viewer--viewer-live-source-p owned))
      (diagram-viewer-viewer-test--deliver (car requests))
      (should (diagram-viewer--viewer-live-source-p owned)))))

(ert-deftest diagram-viewer-viewer-success-displays-bytes-not-artifact ()
  "Success retains a private copy and never visits the artifact path."
  (diagram-viewer-viewer-test--with-viewer
    (let ((result (diagram-viewer-viewer-test--deliver (car requests))))
      (should (eq diagram-viewer--viewer-state 'succeeded))
      (should (equal diagram-viewer--viewer-bytes (diagram-viewer-result-artifact-data result)))
      (should-not (eq diagram-viewer--viewer-bytes (diagram-viewer-result-artifact-data result)))
      (should (equal diagram-viewer--viewer-cache-key (make-string 64 ?a)))
      (should-not buffer-file-name)
      (should (image-get-display-property)))))

(ert-deftest diagram-viewer-viewer-reuses-live-source-identity ()
  "A second descriptor for the same range reuses the viewer."
  (diagram-viewer-viewer-test--with-viewer
    (let* ((old diagram-viewer--viewer-source)
           (again (diagram-viewer--view source)))
      (should (eq again viewer))
      (should (= (length requests) 2))
      (should (= (length cancelled) 1))
      (should-not (marker-buffer (car (diagram-viewer-source-range old)))))))

(ert-deftest diagram-viewer-viewer-adapter-identity-distinguishes-viewers ()
  "Equal positions from different adapters are not the same viewer."
  (diagram-viewer-viewer-test--with-viewer
    (setf (diagram-viewer-source-origin-adapter-id source) 'other)
    (should-not (eq viewer (diagram-viewer--view source)))))

(ert-deftest diagram-viewer-viewer-dead-markers-not-reused ()
  "A viewer with detached markers cannot be matched by integer positions."
  (diagram-viewer-viewer-test--with-viewer
    (diagram-viewer--release-source diagram-viewer--viewer-source)
    (should-not (eq viewer (diagram-viewer--view source)))))

(ert-deftest diagram-viewer-viewer-window-reuse-policy ()
  "Enabling window reuse preserves the visible viewer window."
  (diagram-viewer-viewer-test--with-viewer
    (let ((window (selected-window)) (diagram-viewer-reuse-window t))
      (setf (diagram-viewer-source-origin-adapter-id source) 'other)
      (let ((other (diagram-viewer--view source)))
        (should (eq (selected-window) window))
        (should (eq (window-buffer window) other))))))

(ert-deftest diagram-viewer-viewer-window-reuse-disabled ()
  "Disabling reuse delegates display to the normal buffer display machinery."
  (diagram-viewer-viewer-test--with-viewer
    (let ((diagram-viewer-reuse-window nil) popped)
      (cl-letf (((symbol-function 'pop-to-buffer) (lambda (buffer &rest _) (setq popped buffer))))
        (diagram-viewer--viewer-pop viewer)
        (should (eq popped viewer))))))

(ert-deftest diagram-viewer-viewer-latest-generation-wins ()
  "An old callback cannot replace the rendering screen of a new request."
  (diagram-viewer-viewer-test--with-viewer
    (let ((old (car requests)))
      (diagram-viewer-rerender)
      (diagram-viewer-viewer-test--deliver old)
      (should (eq diagram-viewer--viewer-state 'rendering))
      (should-not diagram-viewer--viewer-bytes)
      (diagram-viewer-viewer-test--deliver (car requests))
      (should (eq diagram-viewer--viewer-state 'succeeded)))))

(ert-deftest diagram-viewer-viewer-request-identity-checked ()
  "A wrong request ID is ignored even within the same generation."
  (diagram-viewer-viewer-test--with-viewer
    (diagram-viewer--viewer-receive
     viewer diagram-viewer--viewer-generation
     (diagram-viewer-result-create :request-id 999 :state 'succeeded))
    (should (eq diagram-viewer--viewer-state 'rendering))))

(ert-deftest diagram-viewer-viewer-stale-result-suppressed ()
  "A Core stale flag prevents native image display."
  (diagram-viewer-viewer-test--with-viewer
    (diagram-viewer-viewer-test--deliver (car requests) 'succeeded t)
    (should (eq diagram-viewer--viewer-state 'stale))
    (should-not diagram-viewer--viewer-bytes)
    (should (string-match-p "suppressed" (buffer-string)))))

(ert-deftest diagram-viewer-viewer-source-current-rechecked ()
  "Changes after Core's stale check are caught by the viewer too."
  (diagram-viewer-viewer-test--with-viewer
    (with-current-buffer source-buffer (goto-char (point-min)) (insert "changed"))
    (diagram-viewer-viewer-test--deliver (car requests))
    (should (eq diagram-viewer--viewer-state 'stale))
    (should-not (image-get-display-property))))

(ert-deftest diagram-viewer-viewer-failure-screen-and-diagnostic ()
  "Failure retains structured diagnostics and advertises g and log access."
  (diagram-viewer-viewer-test--with-viewer
    (diagram-viewer-viewer-test--deliver
     (car requests) 'failed nil '(:id 42 :message "Renderer failed" :stderr "failure tail"))
    (should (eq diagram-viewer--viewer-state 'failed))
    (should (= (plist-get diagram-viewer--viewer-error :id) 42))
    (should (string-match-p "Renderer failed" (buffer-string)))
    (should (string-match-p "diagram-viewer-show-log" (buffer-string)))
    (should-not diagram-viewer--viewer-bytes)))

(ert-deftest diagram-viewer-viewer-rerender-refreshes-owner-descriptor ()
  "Refresh receives viewer-owned markers and supplies the latest source text."
  (diagram-viewer-viewer-test--with-viewer
    (let ((owner diagram-viewer--viewer-source) received fresh)
      (setf (diagram-viewer-source-refresh-function owner)
            (lambda (descriptor)
              (setq received descriptor)
              (with-current-buffer (diagram-viewer-source-buffer descriptor)
                (setq fresh (diagram-viewer-viewer-test--source)))))
      (with-current-buffer source-buffer (goto-char (point-max)) (insert "B-->C\n"))
      (diagram-viewer-rerender)
      (should (eq received owner))
      (should (string-match-p "B-->C" (diagram-viewer-request-source (car requests))))
      (should (diagram-viewer-request-bypass-cache (car requests)))
      (should (= diagram-viewer--viewer-generation 2))
      (should-not (marker-buffer (car (diagram-viewer-source-range owner))))
      (should-not (marker-buffer (car (diagram-viewer-source-range fresh)))))))

(ert-deftest diagram-viewer-viewer-snapshot-rerender-only-if-current ()
  "Without refresh, a current snapshot works but an edited one is refused."
  (diagram-viewer-viewer-test--with-viewer
    (diagram-viewer-viewer-test--deliver (car requests))
    (diagram-viewer-rerender)
    (should (diagram-viewer-request-bypass-cache (car requests)))
    (with-current-buffer source-buffer (goto-char (point-min)) (insert "changed"))
    (let ((count (length requests)))
      (should-error (diagram-viewer-rerender) :type 'user-error)
      (should (= count (length requests))))))

(ert-deftest diagram-viewer-viewer-refresh-target-missing ()
  "A failed extraction is not silently replaced by old source text."
  (diagram-viewer-viewer-test--with-viewer
    (setf (diagram-viewer-source-refresh-function diagram-viewer--viewer-source)
          (lambda (_) (signal 'diagram-viewer-source-not-found '("Removed fence"))))
    (should-error (diagram-viewer-rerender) :type 'diagram-viewer-source-not-found)
    (should (= (length requests) 1))))

(ert-deftest diagram-viewer-viewer-kill-cancels-only-own-request ()
  "Killing a viewer cancels its request and releases only its markers."
  (diagram-viewer-viewer-test--with-viewer
    (let ((owned diagram-viewer--viewer-source) (request (car requests)))
      (kill-buffer viewer)
      (should (equal cancelled (list request)))
      (should-not (marker-buffer (car (diagram-viewer-source-range owned))))
      (should (marker-buffer (car (diagram-viewer-source-range source))))
      (should-not (diagram-viewer--viewer-receive
                   viewer 1 (diagram-viewer-result-create :request-id 1))))))

(ert-deftest diagram-viewer-viewer-source-kill-and-range-loss ()
  "Source navigation and rerender refuse dead buffers and empty ranges."
  (diagram-viewer-viewer-test--with-viewer
    (with-current-buffer source-buffer (erase-buffer))
    (should-error (diagram-viewer-visit-source) :type 'diagram-viewer-source-not-found)
    (should-error (diagram-viewer-rerender) :type 'diagram-viewer-source-not-found)
    (kill-buffer source-buffer)
    (should-error (diagram-viewer-visit-source) :type 'diagram-viewer-source-not-found)))

(ert-deftest diagram-viewer-viewer-visit-source ()
  "Source navigation selects the actual source buffer at its marker."
  (diagram-viewer-viewer-test--with-viewer
    (diagram-viewer-visit-source)
    (should (eq (current-buffer) source-buffer))
    (should (= (point) 1))))

(ert-deftest diagram-viewer-viewer-image-commands-guard-state ()
  "All image-only commands explain why a rendering screen cannot transform."
  (diagram-viewer-viewer-test--with-viewer
    (dolist (command '(diagram-viewer-scale-increase diagram-viewer-scale-decrease
                      diagram-viewer-scale-reset diagram-viewer-fit-to-window))
      (should-error (funcall command) :type 'user-error))
    (should-error (diagram-viewer-save-as "unused.svg") :type 'user-error)))

(ert-deftest diagram-viewer-viewer-transform-native-dispatch ()
  "Image operations delegate to native transforms rather than editing bytes."
  (diagram-viewer-viewer-test--with-viewer
    (diagram-viewer-viewer-test--deliver (car requests))
    (let (calls)
      (cl-letf (((symbol-function 'image-increase-size) (lambda () (push '+ calls)))
                ((symbol-function 'image-decrease-size) (lambda () (push '- calls)))
                ((symbol-function 'image-transform-reset-to-original) (lambda () (push 'reset calls)))
                ((symbol-function 'image-transform-fit-to-window) (lambda () (push 'fit calls)))
                ((symbol-function 'display-graphic-p) (lambda (&rest _) t)))
        (diagram-viewer-scale-increase)
        (diagram-viewer-scale-decrease)
        (diagram-viewer-scale-reset)
        (diagram-viewer-fit-to-window)
        (should (equal (reverse calls) '(+ - reset fit)))))))

(ert-deftest diagram-viewer-viewer-save-bytes-without-cache ()
  "Saving uses displayed bytes even when the artifact does not exist."
  (diagram-viewer-viewer-test--with-viewer
    (diagram-viewer-viewer-test--deliver (car requests))
    (let ((file (make-temp-file "diagram-viewer-save-")))
      (unwind-protect
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (diagram-viewer-save-as file)
            (should (equal diagram-viewer--viewer-bytes
                           (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (insert-file-contents-literally file)
                             (buffer-string)))))
        (delete-file file)))))

(ert-deftest diagram-viewer-viewer-save-confirms-overwrite ()
  "Declining overwrite does not modify the destination."
  (diagram-viewer-viewer-test--with-viewer
    (diagram-viewer-viewer-test--deliver (car requests))
    (let ((file (make-temp-file "diagram-viewer-save-" nil nil "original")) asked)
      (unwind-protect
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (prompt) (setq asked prompt) nil)))
            (should-error (diagram-viewer-save-as file) :type 'user-error)
            (should asked)
            (should (equal (with-temp-buffer (insert-file-contents file) (buffer-string)) "original")))
        (delete-file file)))))

(ert-deftest diagram-viewer-viewer-keymap ()
  "Every approved command has the MVP key and q remains non-destructive."
  (dolist (binding '(("q" . quit-window) ("g" . diagram-viewer-rerender)
                     ("o" . diagram-viewer-visit-source) ("s" . diagram-viewer-save-as)
                     ("+" . diagram-viewer-scale-increase) ("-" . diagram-viewer-scale-decrease)
                     ("0" . diagram-viewer-scale-reset) ("f" . diagram-viewer-fit-to-window)))
    (should (eq (lookup-key diagram-viewer-mode-map (kbd (car binding))) (cdr binding)))))

(ert-deftest diagram-viewer-viewer-show-log-id-and-streams ()
  "Diagnostics remain accessible by ID with separate stdout and stderr tails."
  (save-window-excursion
    (let ((diagram-viewer--diagnostics
           '((:id 7 :condition diagram-viewer-process-failed :message "Failed"
                   :stdout "output tail" :stderr "error tail"))) buffer)
      (unwind-protect
          (progn
            (setq buffer (diagram-viewer--show-log 7))
            (with-current-buffer buffer
              (should buffer-read-only)
              (should (string-match-p "output tail" (buffer-string)))
              (should (string-match-p "error tail" (buffer-string)))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest diagram-viewer-viewer-no-svg-display-is-explained ()
  "A terminal cannot pass checked bytes into an unsupported decoder."
  (diagram-viewer-viewer-test--with-viewer
    (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) nil)))
      (diagram-viewer-viewer-test--deliver (car requests))
      (should (eq diagram-viewer--viewer-state 'failed))
      (should (string-match-p "does not support SVG" (buffer-string))))))

(ert-deftest diagram-viewer-viewer-invalid-refresh-releases-new-markers ()
  "Even invalid refreshed descriptors relinquish their owned markers."
  (diagram-viewer-viewer-test--with-viewer
    (let ((fresh (diagram-viewer--copy-source source)))
      (set-marker (cdr (diagram-viewer-source-range fresh))
                  (car (diagram-viewer-source-range fresh)))
      (setf (diagram-viewer-source-refresh-function diagram-viewer--viewer-source)
            (lambda (_) fresh))
      (should-error (diagram-viewer-rerender) :type 'diagram-viewer-source-not-found)
      (should-not (marker-buffer (car (diagram-viewer-source-range fresh))))
      (should-not (marker-buffer (cdr (diagram-viewer-source-range fresh)))))))

(ert-deftest diagram-viewer-viewer-rejects-nonbyte-success ()
  "A success lacking the checked byte contract never reaches the decoder."
  (diagram-viewer-viewer-test--with-viewer
    (should-error (diagram-viewer--viewer-display (string #x3042) nil)
                  :type 'diagram-viewer-invalid-output)
    (should-error (diagram-viewer--viewer-display nil nil)
                  :type 'diagram-viewer-invalid-output)))

(ert-deftest diagram-viewer-viewer-short-failure-summary ()
  "A failure screen has a short first-line summary, not the entire log."
  (diagram-viewer-viewer-test--with-viewer
    (diagram-viewer--viewer-screen
     'failed (concat (make-string 400 ?x) "\nsecond line") '(:id 1))
    (should-not (string-match-p "second line" (buffer-string)))
    (goto-char (point-min))
    (forward-line 2)
    (should (<= (length (buffer-substring (line-beginning-position) (line-end-position))) 200))))

(defmacro diagram-viewer-viewer-test--with-core-entry (stage &rest body)
  "Run BODY using real Core entry and an inert renderer reentering at STAGE.
Set the lexical variable action to the one-shot renderer callback."
  (declare (indent 1) (debug t))
  `(let ((real-submit (symbol-function 'diagram-viewer-submit))
         (real-cancel (symbol-function 'diagram-viewer-cancel)))
     (diagram-viewer-viewer-test--with-viewer
       (let ((diagram-viewer--renderers (make-hash-table :test #'eq))
             (diagram-viewer--requests (make-hash-table :test #'eql))
             (diagram-viewer--jobs (make-hash-table :test #'equal))
             (diagram-viewer--stopping-jobs (make-hash-table :test #'equal))
             (diagram-viewer--queue nil) (diagram-viewer--active-count 1)
             (diagram-viewer-max-concurrent-jobs 1)
             (diagram-viewer--events nil) (diagram-viewer--event-timer nil)
             (diagram-viewer--next-id 100)
             (entry-stage ,stage) action entered (accept-p nil))
         (cl-letf (((symbol-function 'diagram-viewer-submit) real-submit)
                   ((symbol-function 'diagram-viewer-cancel) real-cancel))
           (unwind-protect
               (progn
                 (diagram-viewer-register-renderer
                  (diagram-viewer-renderer-create
                   :id 'fake :languages '(mermaid) :output-extension ".svg"
                   :available-p
                   (lambda ()
                     (when (and (eq entry-stage 'available) (not entered))
                       (setq entered t) (funcall action))
                     (if (and (eq entry-stage 'available) (not accept-p))
                         '(:message "Inert unavailable renderer") t))
                   :cache-context-function
                   (lambda (_)
                     (when (and (eq entry-stage 'context) (not entered))
                       (setq entered t) (funcall action))
                     (if accept-p '(:context (:version 1))
                       (signal 'diagram-viewer-renderer-unavailable '("Inert context stop"))))
                   :render-function (lambda (&rest _) (ert-fail "Unexpected job"))))
                 ,@body)
             (diagram-viewer--drain-events)))))))

(ert-deftest diagram-viewer-viewer-submit-reentry-keeps-latest-request ()
  "Availability and context reentry cannot install an obsolete request."
  (dolist (stage '(available context))
    (diagram-viewer-viewer-test--with-core-entry stage
      (setq action (lambda () (diagram-viewer--viewer-start source 'fake t)))
      (diagram-viewer--viewer-start source 'fake nil)
      (should (= diagram-viewer--viewer-generation 3))
      (should (= (diagram-viewer-request-id diagram-viewer--viewer-request) 102))
      (diagram-viewer--drain-events)
      (should (eq diagram-viewer--viewer-state 'failed))
      (should (= (hash-table-count diagram-viewer--requests) 0)))))

(ert-deftest diagram-viewer-viewer-submit-kill-cancels-returned-request ()
  "A request returned after renderer-triggered viewer kill is cancelled."
  (dolist (stage '(available context))
    (diagram-viewer-viewer-test--with-core-entry stage
      (setq accept-p t action (lambda () (kill-buffer viewer)))
      (diagram-viewer--viewer-start source 'fake t)
      (should-not (buffer-live-p viewer))
      (let ((request (gethash 101 diagram-viewer--requests)))
        (should request)
        (should (eq (diagram-viewer-request-state request) 'cancelled)))
      (should (= (hash-table-count diagram-viewer--jobs) 0))
      (diagram-viewer--drain-events)
      (should (= (hash-table-count diagram-viewer--requests) 0)))))

(ert-deftest diagram-viewer-viewer-submit-mode-change-keeps-new-mode ()
  "A late submit return cannot install viewer state after a mode change."
  (dolist (stage '(available context))
    (diagram-viewer-viewer-test--with-core-entry stage
      (setq accept-p t action (lambda () (fundamental-mode)))
      (diagram-viewer--viewer-start source 'fake t)
      (should (eq major-mode 'fundamental-mode))
      (should (eq (diagram-viewer-request-state (gethash 101 diagram-viewer--requests))
                  'cancelled))
      (should-not diagram-viewer--viewer-request)
      (should-not (local-variable-p 'diagram-viewer--viewer-request))
      (diagram-viewer--drain-events)
      (should (= (hash-table-count diagram-viewer--requests) 0)))))

(ert-deftest diagram-viewer-viewer-cancel-reentry-does-not-overwrite-latest ()
  "A cancellation callback may start a newer render without being overwritten."
  (diagram-viewer-viewer-test--with-viewer
    (let (entered)
      (cl-letf (((symbol-function 'diagram-viewer-cancel)
                 (lambda (request)
                   (push request cancelled)
                   (unless entered
                     (setq entered t)
                     (diagram-viewer--viewer-start source 'fake t)))))
        (diagram-viewer--viewer-start source 'fake nil)
        (should (= (length requests) 2))
        (should (= diagram-viewer--viewer-generation 3))
        (should (eq diagram-viewer--viewer-request (car requests)))
        (diagram-viewer-viewer-test--deliver (car requests))
        (should (eq diagram-viewer--viewer-state 'succeeded))))))

(ert-deftest diagram-viewer-viewer-renderer-cancel-reentry-keeps-latest ()
  "A real Core renderer cancellation callback may replace the transaction."
  (diagram-viewer-viewer-test--with-core-entry 'available
    (let* ((request diagram-viewer--viewer-request)
           (key (make-string 64 ?c)) called job
           (renderer
            (diagram-viewer-renderer-create
             :cancel-function
             (lambda (_)
               (setq called t)
               (diagram-viewer--viewer-start source 'fake t)
               (diagram-viewer--complete-job job '(:status cancelled))))))
      (setq action #'ignore
            job (diagram-viewer--job-create
                 :key key :generation (make-symbol "cancel-reentry")
                 :renderer renderer :request request :state 'running
                 :waiters (list request) :handle t :handle-ready-p t :counted-p t))
      (puthash key job diagram-viewer--jobs)
      (setf (diagram-viewer-request-state request) 'running
            (diagram-viewer-request-job request) job)
      (diagram-viewer--viewer-start source 'fake nil)
      (should called)
      (should (= diagram-viewer--viewer-generation 3))
      (should (= (diagram-viewer-request-id diagram-viewer--viewer-request) 101))
      (diagram-viewer--drain-events)
      (should (eq diagram-viewer--viewer-state 'failed))
      (should (= diagram-viewer--active-count 0))
      (should (= (hash-table-count diagram-viewer--requests) 0)))))

(ert-deftest diagram-viewer-viewer-cleanup-blocks-cancel-reentry ()
  "Cleanup clears ownership before calling a reentrant cancellation function."
  (diagram-viewer-viewer-test--with-viewer
    (let ((owned diagram-viewer--viewer-source) rejected)
      (cl-letf (((symbol-function 'diagram-viewer-cancel)
                 (lambda (_)
                   (should-not diagram-viewer--viewer-source)
                   (should-not (marker-buffer (car (diagram-viewer-source-range owned))))
                   (setq rejected
                         (should-error (diagram-viewer--viewer-start source 'fake nil)
                                       :type 'user-error)))))
        (kill-buffer viewer)
        (should rejected)))))

(ert-deftest diagram-viewer-viewer-validity-reentry-does-not-change-new-screen ()
  "An adapter predicate cannot let an old delivery replace a newer render."
  (diagram-viewer-viewer-test--with-viewer
    (let ((request (car requests)) entered)
      (setf (diagram-viewer-source-valid-p
             (car (diagram-viewer-request-source-snapshot request)))
            (lambda (_)
              (unless entered
                (setq entered t)
                (diagram-viewer--viewer-start source 'fake t))
              t))
      (diagram-viewer-viewer-test--deliver request)
      (should (eq diagram-viewer--viewer-state 'rendering))
      (should (= (length requests) 2)))))

(ert-deftest diagram-viewer-viewer-refresh-reentry-releases-unused-source ()
  "An obsolete refresh does not overwrite a new render and releases its result."
  (diagram-viewer-viewer-test--with-viewer
    (let ((fresh (diagram-viewer--copy-source source)))
      (setf (diagram-viewer-source-refresh-function diagram-viewer--viewer-source)
            (lambda (_)
              (diagram-viewer--viewer-start source 'fake t)
              fresh))
      (diagram-viewer-rerender)
      (should (= (length requests) 2))
      (should-not (marker-buffer (car (diagram-viewer-source-range fresh)))))))

(ert-deftest diagram-viewer-viewer-native-gui-fixture ()
  "Decode only a Core-validated fixture in an independent graphical Emacs."
  (skip-unless (and (display-graphic-p) (display-images-p) (image-type-available-p 'svg)))
  (require 'diagram-viewer-svg)
  (save-window-excursion
    (let* ((bytes (diagram-viewer-svg-validate-file
                   (expand-file-name "test/fixtures/mermaid-flowchart.svg")))
           (viewer (generate-new-buffer " *viewer native fixture*")))
      (unwind-protect
          (progn
            (pop-to-buffer viewer)
            (diagram-viewer-mode)
            (diagram-viewer--viewer-display bytes (make-string 64 ?b))
            (redisplay t)
            (should (eq major-mode 'diagram-viewer-mode))
            (should (image-get-display-property))
            (should (equal bytes diagram-viewer--viewer-bytes))
            (should-not buffer-file-name)
            (let ((size (image-size (image-get-display-property) t)))
              (should (> (car size) 0))
              (should (> (cdr size) 0)))
            (diagram-viewer-scale-increase)
            (diagram-viewer-scale-decrease)
            (diagram-viewer-scale-reset)
            (diagram-viewer-fit-to-window)
            (should (image-get-display-property))
            (let ((file (make-temp-file "diagram-viewer-native-save-")))
              (unwind-protect
                  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
                    (diagram-viewer-save-as file)
                    (should (equal bytes
                                   (with-temp-buffer
                                     (set-buffer-multibyte nil)
                                     (insert-file-contents-literally file)
                                     (buffer-string)))))
                (delete-file file)))
            (diagram-viewer--viewer-screen 'rendering "Rendering again")
            (should-not (image-get-display-property))
            (diagram-viewer--viewer-display bytes (make-string 64 ?c))
            (should (image-get-display-property))
            (should (eq (key-binding (kbd "s")) #'diagram-viewer-save-as)))
        (kill-buffer viewer)))))

(defun diagram-viewer-viewer-test-run-gui ()
  "Verify the viewer in a fresh graphical Emacs, report results and exit.
Invoke this function only in a separate timeout-controlled Emacs process."
  (condition-case err
      (progn
        (unless (display-graphic-p) (error "A graphical display is required"))
        (let ((stats (ert-run-tests-batch "^diagram-viewer-viewer-")))
          (princ (format "Viewer GUI: %d expected, %d unexpected, %d skipped\n"
                         (ert-stats-completed-expected stats)
                         (ert-stats-completed-unexpected stats) (ert-stats-skipped stats))
                 'external-debugging-output)
          (dolist (test (ert-select-tests "^diagram-viewer-viewer-" t))
            (let ((result (ert-test-most-recent-result test)))
              (when (ert-test-failed-p result)
                (princ (format "%s: %S\n" (ert-test-name test)
                               (ert-test-result-with-condition-condition result))
                       'external-debugging-output))))
          (kill-emacs (if (and (zerop (ert-stats-completed-unexpected stats))
                              (zerop (ert-stats-skipped stats))) 0 1))))
    (error (princ (format "Viewer GUI runner error: %S\n" err) 'external-debugging-output)
           (kill-emacs 2))))

;;; diagram-viewer-viewer-test.el ends here
