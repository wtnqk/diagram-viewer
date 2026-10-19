;;; diagram-viewer-mermaid-test.el --- Mermaid boundary tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Fake process boundary tests need no mmdc/browser.  Opt into the separate
;; real integration test with DIAGRAM_VIEWER_REAL_MERMAID=1.  A missing CLI is
;; skipped; an installed CLI which fails to render is a test failure.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'diagram-viewer-adapter-mermaid)

(defmacro diagram-viewer-mermaid-test--fake (&rest body)
  "Run BODY with a fake process boundary and real temporary input files."
  (declare (indent 0) (debug t))
  `(let* ((diagram-viewer-mermaid-command (executable-find "true"))
          (diagram-viewer-mermaid-browser-executable nil)
          (diagram-viewer-mermaid-theme "default")
          (diagram-viewer-mermaid-background-color "transparent")
          (diagram-viewer-mermaid-extra-arguments nil)
          (diagram-viewer-mermaid-timeout 120)
          (diagram-viewer-mermaid-log-limit 16)
          (snapshot (diagram-viewer-mermaid-cache-context nil))
          (request (diagram-viewer-request-create
                    :source "flowchart TD\n A[日本語] --> B\n"
                    :context (plist-get snapshot :context)
                    :execution-plan (plist-get snapshot :execution-plan)))
          (output (make-temp-file "diagram-viewer-test-output-" nil ".svg"))
          main-args pipe-args child-env handle outcomes
          (main-status 'run) (pipe-status 'open) (exit-code 0) (deletions 0))
     (unwind-protect
         (cl-letf (((symbol-function 'make-pipe-process)
                    (lambda (&rest args) (setq pipe-args args) 'fake-pipe))
                   ((symbol-function 'make-process)
                    (lambda (&rest args)
                      (setq main-args args child-env (copy-sequence process-environment)) 'fake-main))
                   ((symbol-function 'processp)
                    (lambda (value) (memq value '(fake-main fake-pipe))))
                   ((symbol-function 'process-status)
                    (lambda (process) (if (eq process 'fake-main) main-status pipe-status)))
                   ((symbol-function 'process-exit-status) (lambda (_) exit-code))
                   ((symbol-function 'set-process-sentinel) (lambda (&rest _) nil))
                   ((symbol-function 'delete-process) (lambda (_) (cl-incf deletions))))
           (setq handle (diagram-viewer-mermaid-render request output
                                                      (lambda (result) (push result outcomes))))
           (ignore pipe-args main-args child-env main-status pipe-status exit-code deletions outcomes)
           ,@body)
       (when handle (diagram-viewer-mermaid-cancel handle))
       (when (file-exists-p output) (delete-file output)))))

(ert-deftest diagram-viewer-mermaid-register-without-cli ()
  (let ((diagram-viewer--renderers (make-hash-table :test #'eq))
        (diagram-viewer-mermaid-command "/not/installed/mmdc"))
    (should (eq (diagram-viewer-mermaid-enable) 'mermaid))
    (should (diagram-viewer-get-renderer 'mermaid))
    (should (eq (plist-get (diagram-viewer-mermaid-available-p) :condition)
                'diagram-viewer-renderer-unavailable))))

(ert-deftest diagram-viewer-mermaid-availability-no-subprocess ()
  (let ((diagram-viewer-mermaid-command (executable-find "true"))
        (diagram-viewer-mermaid-browser-executable nil))
    (cl-letf (((symbol-function 'make-process) (lambda (&rest _) (ert-fail "spawned"))))
      (should (eq t (diagram-viewer-mermaid-available-p))))))

(ert-deftest diagram-viewer-mermaid-browser-executable-validation ()
  (let ((diagram-viewer-mermaid-command (executable-find "true")))
    (dolist (path '("relative/chrome" "/missing/chrome" "/tmp/" "/ssh:host:/bin/chrome"))
      (let ((diagram-viewer-mermaid-browser-executable path))
        (should-not (eq t (diagram-viewer-mermaid-available-p)))))))

(ert-deftest diagram-viewer-mermaid-reject-argv-overrides ()
  (let ((diagram-viewer-mermaid-command (executable-find "true"))
        (diagram-viewer-mermaid-browser-executable nil))
    (dolist (arg '("-i" "--input=x" "-o" "--output=x" "-s" "--scale=2"
                   "--configFile=x" "--puppeteerConfigFile=x" "--quiet" "--"))
      (should-error (diagram-viewer-mermaid-cache-context (list :extra-arguments (list arg)))
                    :type 'diagram-viewer-invalid-argument))))

(ert-deftest diagram-viewer-mermaid-invalid-options ()
  (let ((diagram-viewer-mermaid-command (executable-find "true"))
        (diagram-viewer-mermaid-browser-executable nil))
    (dolist (options '((:unknown t) (:theme "bad") (:background "red; evil")
                      (:theme "dark" :theme "forest") (:extra-arguments "-o")))
      (should-error (diagram-viewer-mermaid-cache-context options)
                    :type 'diagram-viewer-invalid-argument))))

(ert-deftest diagram-viewer-mermaid-config-and-utf8-input ()
  (diagram-viewer-mermaid-test--fake
    (let ((argv (plist-get main-args :command)))
      (should (equal (cadr (member "--output" argv)) output))
      (should (equal (cadr (member "--outputFormat" argv)) "svg"))
      (should (eq (plist-get main-args :connection-type) 'pipe))
      (should (eq (plist-get main-args :coding) 'binary))
      (should (plist-get main-args :noquery))
      (should-not (member "--scale" argv))
      (with-temp-buffer
        (insert-file-contents (cadr (member "--input" argv)))
        (should (equal (buffer-string) (diagram-viewer-request-source request))))
      (with-temp-buffer
        (insert-file-contents (cadr (member "--configFile" argv)))
        (should (string-match-p "\"htmlLabels\":false" (buffer-string)))))))

(ert-deftest diagram-viewer-mermaid-settings-snapshot ()
  (diagram-viewer-mermaid-test--fake
    (let ((diagram-viewer-mermaid-theme "dark")
          (diagram-viewer-mermaid-command "/bad/new/mmdc")
          (diagram-viewer-mermaid-timeout 1)
          second)
      (setq second (diagram-viewer-mermaid-render request output #'ignore))
      (should (equal (car (plist-get main-args :command)) (executable-find "true")))
      (should (equal (cadr (member "--theme" (plist-get main-args :command))) "default"))
      (diagram-viewer-mermaid-cancel second))))

(ert-deftest diagram-viewer-mermaid-environment-inherited ()
  (let ((process-environment (cons "PUPPETEER_EXECUTABLE_PATH=/inherited/browser"
                                   process-environment)))
    (diagram-viewer-mermaid-test--fake
      (should (equal child-env process-environment)))))

(ert-deftest diagram-viewer-mermaid-environment-child-only ()
  (diagram-viewer-mermaid-test--fake
    (let* ((original (copy-sequence process-environment))
           (browser (executable-find "true"))
           (plan (diagram-viewer-request-execution-plan request)) second)
      (setf (diagram-viewer-request-execution-plan request)
            (plist-put plan :browser browser))
      (setq second (diagram-viewer-mermaid-render request output #'ignore))
      (should (member (concat "PUPPETEER_EXECUTABLE_PATH=" browser) child-env))
      (should (equal original process-environment))
      (diagram-viewer-mermaid-cancel second))))

(ert-deftest diagram-viewer-mermaid-stderr-drains-before-complete ()
  (diagram-viewer-mermaid-test--fake
    (setq main-status 'exit exit-code 3)
    (funcall (plist-get main-args :sentinel) 'fake-main "exited")
    (should-not outcomes)
    (funcall (plist-get pipe-args :filter) 'fake-pipe "last stderr")
    (setq pipe-status 'closed)
    (funcall (plist-get pipe-args :sentinel) 'fake-pipe "closed")
    (should (equal (plist-get (plist-get (car outcomes) :error) :stderr) "last stderr"))
    (should (= (plist-get (plist-get (car outcomes) :error) :exit-code) 3))))

(ert-deftest diagram-viewer-mermaid-success-cleanup-once ()
  (diagram-viewer-mermaid-test--fake
    (setq main-status 'exit pipe-status 'closed)
    (funcall (plist-get main-args :sentinel) 'fake-main "done")
    (funcall (plist-get pipe-args :sentinel) 'fake-pipe "closed")
    (diagram-viewer-mermaid-cancel handle)
    (should (equal outcomes '((:status succeeded))))
    (should (= deletions 2))
    (should-not (file-exists-p (diagram-viewer-mermaid--handle-input handle)))
    (should-not (file-exists-p (diagram-viewer-mermaid--handle-config handle)))
    (should (file-exists-p output))))

(ert-deftest diagram-viewer-mermaid-cancel-before-sentinel ()
  (diagram-viewer-mermaid-test--fake
    (diagram-viewer-mermaid-cancel handle)
    (setq main-status 'signal pipe-status 'closed exit-code 9)
    (funcall (plist-get main-args :sentinel) 'fake-main "killed")
    (diagram-viewer-mermaid--stop handle 'timeout)
    (should (= (length outcomes) 1))
    (should (eq (plist-get (car outcomes) :status) 'cancelled))
    (should (= deletions 2))
    (should (file-exists-p output))))

(ert-deftest diagram-viewer-mermaid-timeout-before-cancel ()
  (diagram-viewer-mermaid-test--fake
    (diagram-viewer-mermaid--stop handle 'timeout)
    (diagram-viewer-mermaid-cancel handle)
    (should (= (length outcomes) 1))
    (should (eq (plist-get (car outcomes) :status) 'failed))
    (should (eq (plist-get (plist-get (car outcomes) :error) :reason) 'timeout))))

(ert-deftest diagram-viewer-mermaid-signal-is-failure ()
  (diagram-viewer-mermaid-test--fake
    (setq main-status 'signal pipe-status 'closed exit-code 15)
    (funcall (plist-get main-args :sentinel) 'fake-main "terminated")
    (should (eq (plist-get (car outcomes) :status) 'failed))))

(ert-deftest diagram-viewer-mermaid-byte-log-bound ()
  (diagram-viewer-mermaid-test--fake
    (funcall (plist-get main-args :filter) 'fake-main (make-string 100000 ?x))
    (funcall (plist-get pipe-args :filter) 'fake-pipe "日本語日本語日本語")
    (should (= (length (diagram-viewer-mermaid--handle-stdout handle)) 16))
    (should (= (length (diagram-viewer-mermaid--handle-stderr handle)) 16))
    (funcall (plist-get main-args :filter) 'fake-main "tail")
    (should (string-suffix-p "tail" (diagram-viewer-mermaid--handle-stdout handle)))))

(ert-deftest diagram-viewer-mermaid-startup-failure-cleanup ()
  (diagram-viewer-mermaid-test--fake
    (let (failed failure)
      (cl-letf (((symbol-function 'make-process) (lambda (&rest _) (error "Spawn failed"))))
        (setq failed (diagram-viewer-mermaid-render request output
                                                   (lambda (result) (setq failure result)))))
      (should (eq (plist-get failure :status) 'failed))
      (should (eq (plist-get (plist-get failure :error) :reason) 'startup))
      (should-not (file-exists-p (diagram-viewer-mermaid--handle-input failed)))
      (should-not (file-exists-p (diagram-viewer-mermaid--handle-config failed)))
      (should (file-exists-p output)))))

(ert-deftest diagram-viewer-mermaid-canonical-snapshot-and-limits ()
  (let ((diagram-viewer-mermaid-command (executable-find "true"))
        (diagram-viewer-mermaid-browser-executable nil))
    (should (diagram-viewer--canonical-plist (diagram-viewer-mermaid-cache-context nil)))
    (dolist (timeout '(0 -1 0.5 nil))
      (let ((diagram-viewer-mermaid-timeout timeout))
        (should-error (diagram-viewer-mermaid-cache-context nil)
                      :type 'diagram-viewer-invalid-argument)))
    (let ((diagram-viewer-mermaid-log-limit -1))
      (should-error (diagram-viewer-mermaid-cache-context nil)
                    :type 'diagram-viewer-invalid-argument))))

(ert-deftest diagram-viewer-mermaid-stderr-before-main-sentinel ()
  (diagram-viewer-mermaid-test--fake
    (setq pipe-status 'closed)
    (funcall (plist-get pipe-args :sentinel) 'fake-pipe "closed")
    (should-not outcomes)
    (setq main-status 'exit)
    (funcall (plist-get main-args :sentinel) 'fake-main "done")
    (should (equal outcomes '((:status succeeded))))))

(ert-deftest diagram-viewer-mermaid-real-integration ()
  :tags '(:integration)
  (skip-unless (equal (getenv "DIAGRAM_VIEWER_REAL_MERMAID") "1"))
  (let ((diagram-viewer-mermaid-command
         (or (getenv "DIAGRAM_VIEWER_MMDC") "mmdc"))
        (diagram-viewer-mermaid-browser-executable (getenv "DIAGRAM_VIEWER_BROWSER"))
        (diagram-viewer-mermaid-extra-arguments nil)
        (diagram-viewer-mermaid-timeout 30))
    (skip-unless (eq (diagram-viewer-mermaid-available-p) t))
    (let* ((snapshot (diagram-viewer-mermaid-cache-context nil))
           (request (diagram-viewer-request-create
                     :source (concat "%%{init: {\"htmlLabels\":true,"
                                     "\"flowchart\":{\"htmlLabels\":true}}}%%\n"
                                     "flowchart TD\n A[Safe label] --> B[Result]\n")
                     :context (plist-get snapshot :context)
                     :execution-plan (plist-get snapshot :execution-plan)))
           (output (make-temp-file "diagram-viewer-real-" nil ".svg")) result handle)
      (unwind-protect
          (progn
            (setq handle (diagram-viewer-mermaid-render request output
                                                        (lambda (outcome) (setq result outcome))))
            (while (not result) (accept-process-output nil 0.1))
            (ert-info ((format "Renderer outcome: %S" result))
              (should (eq (plist-get result :status) 'succeeded)))
            (with-temp-buffer
              (insert-file-contents output)
              (should (search-forward "<svg" nil t))
              (should-not (search-forward "foreignObject" nil t))))
        (when handle (diagram-viewer-mermaid-cancel handle))
        (delete-file output)))))

(provide 'diagram-viewer-mermaid-test)
;;; diagram-viewer-mermaid-test.el ends here
