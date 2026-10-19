;;; diagram-viewer-ui-integration-test.el --- Command to viewer integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Batch tests replace only native image display, not Core SVG validation.
;; The GUI runner must be started in a NEW disposable Emacs process, never
;; sent to an existing user server.  It needs real md-mode, mmdc and a browser.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'diagram-viewer-adapter-md-mode)
(require 'diagram-viewer-adapter-mermaid)
(require 'diagram-viewer-viewer)

(defvar diagram-viewer-ui-test--inputs nil)
(defconst diagram-viewer-ui-test--source "```mermaid\nflowchart LR\n A --> B\n```\n")

(defmacro diagram-viewer-ui-test--with-env (&rest body)
  "Run BODY in an isolated Core and viewer environment."
  (declare (indent 0) (debug t))
  `(let* ((diagram-viewer--renderers (make-hash-table :test #'eq))
          (diagram-viewer--source-adapters (make-hash-table :test #'eq))
          (diagram-viewer--jobs (make-hash-table :test #'equal))
          (diagram-viewer--stopping-jobs (make-hash-table :test #'equal))
          (diagram-viewer--requests (make-hash-table :test #'eql))
          (diagram-viewer--queue nil) (diagram-viewer--events nil)
          (diagram-viewer--event-timer nil) (diagram-viewer--pumping nil)
          (diagram-viewer--active-count 0) (diagram-viewer--diagnostics nil)
          (diagram-viewer-default-renderer-alist nil)
          (cache-root (make-temp-file "diagram-viewer-ui-test-" t))
          (diagram-viewer-cache-directory cache-root)
          (source-buffer (generate-new-buffer " *diagram-viewer-ui-source*")))
     (save-window-excursion
       (unwind-protect
           (progn (diagram-viewer-md-mode-enable) ,@body)
         (dolist (buffer (buffer-list))
           (when (with-current-buffer buffer
                   (and (eq major-mode 'diagram-viewer-mode)
                        diagram-viewer--viewer-source
                        (eq source-buffer (diagram-viewer-source-buffer diagram-viewer--viewer-source))))
             (kill-buffer buffer)))
         (when (buffer-live-p source-buffer) (kill-buffer source-buffer))
         (diagram-viewer--drain-events)
         (when (timerp diagram-viewer--event-timer) (cancel-timer diagram-viewer--event-timer))
         (unless (string-prefix-p "diagram-viewer-ui-test-"
                                  (file-name-nondirectory cache-root))
           (error "Unexpected test cache directory"))
         (delete-directory cache-root t)))))

(defun diagram-viewer-ui-test--register-fixture-renderer ()
  "Register an inert SVG renderer while keeping real Core validation."
  (diagram-viewer-register-renderer
   (diagram-viewer-renderer-create
    :id 'fixture :languages '(mermaid) :output-extension ".svg"
    :available-p (lambda () t)
    :cache-context-function (lambda (_) '(:context (:implementation 1)))
    :render-function
    (lambda (request output complete)
      (push (diagram-viewer-request-source request) diagram-viewer-ui-test--inputs)
      (with-temp-file output
        (insert "<svg xmlns='http://www.w3.org/2000/svg' width='120' height='80'><rect width='100' height='60' fill='#6688bb'/></svg>"))
      (funcall complete '(:status succeeded))))))

(defun diagram-viewer-ui-test--batch-display (bytes key)
  "Record checked BYTES and KEY without invoking a graphical decoder."
  (should-not (multibyte-string-p bytes))
  (should (string-prefix-p "<svg" bytes))
  (setq diagram-viewer--viewer-bytes (copy-sequence bytes)
        diagram-viewer--viewer-cache-key key diagram-viewer--viewer-state 'succeeded))

(ert-deftest diagram-viewer-ui-edit-command-refreshes-current-body ()
  (diagram-viewer-ui-test--with-env
   (let (diagram-viewer-ui-test--inputs viewer)
     (diagram-viewer-ui-test--register-fixture-renderer)
     (cl-letf (((symbol-function 'diagram-viewer--viewer-display)
                #'diagram-viewer-ui-test--batch-display))
       (with-current-buffer source-buffer
         (setq major-mode 'md-mode)
         (insert diagram-viewer-ui-test--source)
         (goto-char (point-min))
         (setq viewer (diagram-viewer-view-at-point)))
       (diagram-viewer--drain-events)
       (should (equal "flowchart LR\n A --> B\n" (car diagram-viewer-ui-test--inputs)))
       (with-current-buffer source-buffer
         (goto-char (point-min)) (search-forward "B") (replace-match "C"))
       (with-current-buffer viewer (diagram-viewer-rerender))
       (diagram-viewer--drain-events)
       (should (= 2 (length diagram-viewer-ui-test--inputs)))
       (should (equal "flowchart LR\n A --> C\n" (car diagram-viewer-ui-test--inputs)))
       (with-current-buffer source-buffer
         (goto-char (point-min))
         (should (eq viewer (diagram-viewer-view-at-point))))
       (diagram-viewer--drain-events)
       (should (= 2 (length diagram-viewer-ui-test--inputs)))
       (with-current-buffer viewer
         (should (eq 'succeeded diagram-viewer--viewer-state))
         (should-not buffer-file-name)
         (should buffer-read-only))))))

(ert-deftest diagram-viewer-ui-rendered-command-is-read-only-and-refreshes-properties ()
  (diagram-viewer-ui-test--with-env
   (let (diagram-viewer-ui-test--inputs viewer snapshot undo modified location)
     (diagram-viewer-ui-test--register-fixture-renderer)
     (with-current-buffer source-buffer
       (setq major-mode 'md-mode)
       (insert "\n \n\n")
       (add-text-properties 1 5 `(md-render-source ,diagram-viewer-ui-test--source md-render-frozen t))
       (put-text-property 2 3 'md-render-media-file "/fixture/inline.png")
       (put-text-property 2 3 'display '(image :type png :file "/fixture/never-read.png"))
       (goto-char 2)
       (set-buffer-modified-p nil)
       (setq snapshot (buffer-string) undo buffer-undo-list modified (buffer-modified-p)
             location (point) buffer-read-only t))
     (cl-letf (((symbol-function 'diagram-viewer--viewer-display)
                #'diagram-viewer-ui-test--batch-display))
       (with-current-buffer source-buffer
         (setq viewer (diagram-viewer-md-mode-view-rendered-at-point)))
       (diagram-viewer--drain-events)
       (with-current-buffer source-buffer
         (should (equal-including-properties snapshot (buffer-string)))
         (should (equal undo buffer-undo-list))
         (should (eq modified (buffer-modified-p)))
         (should (= location (point)))
         (let ((inhibit-read-only t))
           (with-silent-modifications
             (put-text-property 1 5 'md-render-source
                                (string-replace "B" "C" diagram-viewer-ui-test--source)))))
       (with-current-buffer viewer (diagram-viewer-rerender))
       (diagram-viewer--drain-events)
       (should (equal "flowchart LR\n A --> C\n" (car diagram-viewer-ui-test--inputs)))
       (with-current-buffer source-buffer
         (should (equal '(image :type png :file "/fixture/never-read.png")
                        (get-text-property 2 'display))))))))

(defun diagram-viewer-ui-test--wait-viewer (viewer)
  "Wait for VIEWER's real request and assert successful native display."
  (let ((deadline (+ (float-time) 40)))
    (while (and (with-current-buffer viewer (eq 'rendering diagram-viewer--viewer-state))
                (< (float-time) deadline))
      (accept-process-output nil 0.05)
      (diagram-viewer--drain-events))
    (with-current-buffer viewer
      (unless (eq diagram-viewer--viewer-state 'succeeded)
        (error "Viewer failed: %S" diagram-viewer--viewer-error))
      (should (image-get-display-property))
      (should-not (multibyte-string-p diagram-viewer--viewer-bytes)))))

(defun diagram-viewer-ui-integration-gui-run ()
  "Run real command, md-mode PNG and SVG checks in a NEW GUI process.
Terminate that process with a success/failure exit code.  Never invoke this
function via an existing Emacs server."
  (condition-case err
      (progn
        (require 'md-mode)
        (unless (display-graphic-p) (error "A graphical frame is required"))
        (sit-for 0.1)
        (let ((diagram-viewer-mermaid-command (or (getenv "DIAGRAM_VIEWER_MMDC") "mmdc"))
              (diagram-viewer-mermaid-browser-executable (getenv "DIAGRAM_VIEWER_BROWSER"))
              (diagram-viewer-mermaid-timeout 30))
          (unless (eq t (diagram-viewer-mermaid-available-p)) (error "Renderer is unavailable"))
          (diagram-viewer-ui-test--with-env
           (let ((md-render-cache-directory (expand-file-name "png/" diagram-viewer-cache-directory))
                 (md-render-mermaid-command diagram-viewer-mermaid-command)
                 (md-render-mermaid-browser diagram-viewer-mermaid-browser-executable)
                 (md-render-mermaid-enabled t) edit-viewer rendered-viewer image-position png)
             (setq diagram-viewer-cache-directory (expand-file-name "svg/" diagram-viewer-cache-directory))
             (make-directory md-render-cache-directory t)
             (diagram-viewer-mermaid-enable)
             (switch-to-buffer source-buffer)
             (insert diagram-viewer-ui-test--source)
             (md-mode)
             (goto-char (point-min))
             (setq edit-viewer (diagram-viewer-view-at-point))
             (diagram-viewer-ui-test--wait-viewer edit-viewer)
             (with-current-buffer edit-viewer (diagram-viewer-visit-source))
             (should (eq (window-buffer (selected-window)) source-buffer))
             (with-current-buffer source-buffer (md-mode-render))
             (let ((deadline (+ (float-time) 40)))
               (while (and (not png) (< (float-time) deadline))
                 (with-current-buffer source-buffer
                   (let ((position (text-property-any (point-min) (point-max) 'md-render-frozen t)))
                     (when position
                       (setq image-position (1+ position))
                       (let ((file (get-text-property image-position 'md-render-media-file)))
                         (when (and file (file-exists-p file)
                                    (get-text-property image-position 'display))
                           (setq png file))))))
                 (unless png (accept-process-output nil 0.05))))
             (should png)
             (with-current-buffer source-buffer
               (goto-char image-position)
               (let ((before (buffer-string)) (modified (buffer-modified-p)) (undo buffer-undo-list))
                 (setq rendered-viewer (diagram-viewer-md-mode-view-rendered-at-point))
                 (with-current-buffer source-buffer
                   (should (equal-including-properties before (buffer-string)))
                   (should (eq modified (buffer-modified-p)))
                   (should (equal undo buffer-undo-list)))))
             (diagram-viewer-ui-test--wait-viewer rendered-viewer)
             (with-current-buffer rendered-viewer
               (diagram-viewer-fit-to-window)
               (diagram-viewer-save-as (expand-file-name "saved.svg" diagram-viewer-cache-directory))
               (diagram-viewer-cache-clear t)
               (diagram-viewer-save-as (expand-file-name "saved-after-clear.svg" diagram-viewer-cache-directory))
               (diagram-viewer-visit-source))
             (with-current-buffer source-buffer
               (md-mode-show-source)
               (should (equal diagram-viewer-ui-test--source (buffer-string))))
             (should (file-exists-p png))
             ;; Restore the owning fixture directory for guarded cleanup.
             (setq diagram-viewer-cache-directory (file-name-directory
                                                   (directory-file-name diagram-viewer-cache-directory))))))
        (princ "UI GUI integration: Edit command, Rendered command, native SVG, inline PNG, save/cache-clear and source reconstruction passed\n" #'external-debugging-output)
        (kill-emacs 0))
    ((error quit)
     (princ (format "UI GUI integration failed: %S\n" err) #'external-debugging-output)
     (kill-emacs 1))))

(provide 'diagram-viewer-ui-integration-test)
;;; diagram-viewer-ui-integration-test.el ends here
