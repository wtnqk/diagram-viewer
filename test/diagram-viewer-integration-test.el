;;; diagram-viewer-integration-test.el --- Core publication integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise the real SVG validator and atomic Core cache publication.
;; The optional browser test is enabled with DIAGRAM_VIEWER_REAL_MERMAID=1.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'diagram-viewer)
(require 'diagram-viewer-adapter-mermaid)

(defmacro diagram-viewer-integration-test--with-env (&rest body)
  "Run BODY with isolated Core state and an owned source descriptor."
  (declare (indent 0) (debug t))
  `(let* ((diagram-viewer--renderers (make-hash-table :test #'eq))
          (diagram-viewer--jobs (make-hash-table :test #'equal))
          (diagram-viewer--stopping-jobs (make-hash-table :test #'equal))
          (diagram-viewer--requests (make-hash-table :test #'eql))
          (diagram-viewer--queue nil) (diagram-viewer--events nil)
          (diagram-viewer--event-timer nil) (diagram-viewer--pumping nil)
          (diagram-viewer--active-count 0) (diagram-viewer--diagnostics nil)
          (diagram-viewer-default-renderer-alist nil)
          (diagram-viewer-max-concurrent-jobs 2)
          (diagram-viewer-cache-directory (make-temp-file "diagram-viewer-integration-" t)))
     (unwind-protect
         (with-temp-buffer
           (insert "flowchart TD\n A[日本語] --> B\n")
           (let ((source (diagram-viewer-source-create
                          :text (buffer-string) :language 'mermaid :buffer (current-buffer)
                          :range (cons (copy-marker 1) (copy-marker (point-max) t)))))
             (unwind-protect (progn ,@body)
               (diagram-viewer--release-source source))))
       (let (pending)
         (maphash (lambda (_ request) (push request pending)) diagram-viewer--requests)
         (mapc #'diagram-viewer-cancel pending)
         (diagram-viewer--drain-events))
       (when (timerp diagram-viewer--event-timer) (cancel-timer diagram-viewer--event-timer))
       (unless (string-prefix-p "diagram-viewer-integration-"
                                (file-name-nondirectory diagram-viewer-cache-directory))
         (error "Unexpected cache cleanup target"))
       (delete-directory diagram-viewer-cache-directory t))))

(ert-deftest diagram-viewer-integration-publishes-only-staticized-bytes ()
  (diagram-viewer-integration-test--with-env
   (let ((calls 0) first second)
     (diagram-viewer-register-renderer
      (diagram-viewer-renderer-create
       :id 'fixture :languages '(mermaid) :output-extension ".svg"
       :available-p (lambda () t)
       :cache-context-function (lambda (_) '(:context (:implementation 1)))
       :render-function
       (lambda (_request output done)
         (cl-incf calls)
         (with-temp-file output
           (insert "<svg xmlns='http://www.w3.org/2000/svg' width='20' height='20'><style>@keyframes a{from{fill:red;}to{fill:blue;}}rect{animation:a 1ms infinite;fill:red;}</style><rect width='10' height='10'/></svg>"))
         (funcall done '(:status succeeded)))))
     (diagram-viewer-submit source :callback (lambda (result) (setq first result)))
     (should-not first)
     (diagram-viewer--drain-events)
     (should (eq 'succeeded (diagram-viewer-result-state first)))
     (let ((data (diagram-viewer-result-artifact-data first))
           (path (diagram-viewer-result-artifact first)))
       (should-not (string-match-p "@keyframes\\|animation:" data))
       (should (equal data (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (insert-file-contents-literally path)
                             (buffer-string)))))
     (diagram-viewer-submit source :callback (lambda (result) (setq second result)))
     (diagram-viewer--drain-events)
     (should (diagram-viewer-result-cache-hit-p second))
     (should (equal (diagram-viewer-result-artifact-data first)
                    (diagram-viewer-result-artifact-data second)))
     (should (= calls 1)))))

(ert-deftest diagram-viewer-integration-rejects-active-output-before-publication ()
  (diagram-viewer-integration-test--with-env
   (let (result)
     (diagram-viewer-register-renderer
      (diagram-viewer-renderer-create
       :id 'malicious :languages '(mermaid) :output-extension ".svg"
       :available-p (lambda () t)
       :cache-context-function (lambda (_) '(:context (:implementation 1)))
       :render-function
       (lambda (_request output done)
         (with-temp-file output
           (insert "<svg xmlns='http://www.w3.org/2000/svg'><script>alert(1)</script></svg>"))
         (funcall done '(:status succeeded)))))
     (diagram-viewer-submit source :callback (lambda (value) (setq result value)))
     (diagram-viewer--drain-events)
     (should (eq 'failed (diagram-viewer-result-state result)))
     (should (eq 'diagram-viewer-invalid-output
                 (plist-get (diagram-viewer-result-error result) :condition)))
     (should-not (directory-files diagram-viewer-cache-directory nil "[.]svg$"))
     (should (= 0 diagram-viewer--active-count)))))

(ert-deftest diagram-viewer-integration-real-mermaid-core-cache ()
  (skip-unless (equal (getenv "DIAGRAM_VIEWER_REAL_MERMAID") "1"))
  (let ((diagram-viewer-mermaid-command (or (getenv "DIAGRAM_VIEWER_MMDC") "mmdc"))
        (diagram-viewer-mermaid-browser-executable (getenv "DIAGRAM_VIEWER_BROWSER"))
        (diagram-viewer-mermaid-timeout 30))
    (skip-unless (eq t (diagram-viewer-mermaid-available-p)))
    (diagram-viewer-integration-test--with-env
     (diagram-viewer-mermaid-enable)
     (let (result cached)
       (diagram-viewer-submit source :callback (lambda (value) (setq result value)))
       (let ((deadline (+ (float-time) 35)))
         (while (and (not result) (< (float-time) deadline))
           (accept-process-output nil 0.05)
           (diagram-viewer--drain-events)))
       (should result)
       (ert-info ((format "Real renderer outcome: %S" (diagram-viewer-result-error result)))
                 (should (eq 'succeeded (diagram-viewer-result-state result))))
       (should-not (string-match-p "foreignObject\\|<script\\|@keyframes\\|animation:"
                                   (diagram-viewer-result-artifact-data result)))
       (diagram-viewer-submit source :callback (lambda (value) (setq cached value)))
       (diagram-viewer--drain-events)
       (should (diagram-viewer-result-cache-hit-p cached))
       (should (= 0 diagram-viewer--active-count))
       (should (= 0 (hash-table-count diagram-viewer--jobs)))))))

(provide 'diagram-viewer-integration-test)
;;; diagram-viewer-integration-test.el ends here
