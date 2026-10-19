;;; diagram-viewer-jobs-test.el --- Scheduler contract tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Inert bytes stand in for SVG.  These tests do NOT validate SVG safety.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'diagram-viewer)

(defvar diagram-viewer-jobs-test--runs nil)

(defun diagram-viewer-jobs-test--renderer (&optional cancel synchronous)
  "Make an inert renderer with CANCEL and optional SYNCHRONOUS completion."
  (diagram-viewer-renderer-create
   :id 'fake :languages '(mermaid) :output-extension ".svg"
   :available-p (lambda () t)
   :cache-context-function (lambda (_) '(:context (:version 1)))
   :cancel-function cancel
   :render-function
   (lambda (request output done)
     (let ((run (list :request request :output output :done done)))
       (setq diagram-viewer-jobs-test--runs
             (append diagram-viewer-jobs-test--runs (list run)))
       (when synchronous (diagram-viewer-jobs-test--complete run))
       run))))

(defun diagram-viewer-jobs-test--validate (path)
  "Read inert fixture PATH; reject non-fixture bytes."
  (unless (and (file-regular-p path) (not (file-symlink-p path)))
    (signal 'diagram-viewer-invalid-output '("Not a regular fixture")))
  (let ((bytes (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (insert-file-contents-literally path)
                 (buffer-string))))
    (unless (string-prefix-p "fixture:" bytes)
      (signal 'diagram-viewer-invalid-output '("Invalid fixture")))
    bytes))

(defun diagram-viewer-jobs-test--complete (run &optional contents outcome)
  "Complete RUN with CONTENTS and OUTCOME, defaulting to inert success."
  (with-temp-file (plist-get run :output)
    (insert (or contents "fixture:one")))
  (funcall (plist-get run :done) (or outcome '(:status succeeded))))

(defmacro diagram-viewer-jobs-test--with-env (&rest body)
  "Run BODY with isolated scheduler state and a source named source."
  (declare (indent 0) (debug t))
  `(let* ((diagram-viewer--renderers (make-hash-table :test #'eq))
          (diagram-viewer--source-adapters (make-hash-table :test #'eq))
          (diagram-viewer--jobs (make-hash-table :test #'equal))
          (diagram-viewer--stopping-jobs (make-hash-table :test #'equal))
          (diagram-viewer--requests (make-hash-table :test #'eql))
          (diagram-viewer--queue nil) (diagram-viewer--events nil)
          (diagram-viewer--event-timer nil) (diagram-viewer--pumping nil)
          (diagram-viewer--active-count 0) (diagram-viewer--next-id 0)
          (diagram-viewer--diagnostics nil)
          (diagram-viewer-default-renderer-alist nil)
          (diagram-viewer-max-concurrent-jobs 2)
          (diagram-viewer-jobs-test--runs nil)
          (diagram-viewer-cache-directory (make-temp-file "diagram-viewer-test-" t)))
     (unwind-protect
         (with-temp-buffer
           (insert "source")
           (let ((source (diagram-viewer-source-create
                          :text "source" :language 'mermaid :adapter-id 'test
                          :buffer (current-buffer)
                          :range (cons (copy-marker (point-min))
                                       (copy-marker (point-max) t)))))
             (cl-letf (((symbol-function 'diagram-viewer--validate-output)
                        #'diagram-viewer-jobs-test--validate))
		      (diagram-viewer-register-renderer (diagram-viewer-jobs-test--renderer))
		      ,@body)))
       (when (timerp diagram-viewer--event-timer)
         (cancel-timer diagram-viewer--event-timer))
       ;; Only remove the fresh directory made by this fixture.
       (unless (and (file-name-absolute-p diagram-viewer-cache-directory)
                    (string-prefix-p "diagram-viewer-test-"
                                     (file-name-nondirectory
                                      (directory-file-name diagram-viewer-cache-directory)) ))
         (error "Unexpected test cleanup target"))
       (delete-directory diagram-viewer-cache-directory t))))

(ert-deftest diagram-viewer-jobs-coalesces-and-delivers-once ()
  (diagram-viewer-jobs-test--with-env
   (let* (results
          (a (diagram-viewer-submit source :callback (lambda (x) (push x results))))
          (b (diagram-viewer-submit source :callback (lambda (x) (push x results)))))
     (should (= 1 (length diagram-viewer-jobs-test--runs)))
     (diagram-viewer-jobs-test--complete (car diagram-viewer-jobs-test--runs))
     (should-not results)
     (should (eq 'succeeded (diagram-viewer-request-state a)))
     (funcall (plist-get (car diagram-viewer-jobs-test--runs) :done) '(:status failed))
     (diagram-viewer--drain-events)
     (should (= 2 (length results)))
     (should (eq 'succeeded (diagram-viewer-request-state b)))
     (should (= 0 diagram-viewer--active-count))
     (should (= 0 (hash-table-count diagram-viewer--jobs)))
     (should (= 0 (hash-table-count diagram-viewer--requests))))))

(ert-deftest diagram-viewer-jobs-cache-hit-is-deferred-and-keeps-bytes ()
  (diagram-viewer-jobs-test--with-env
   (let ((first (diagram-viewer-submit source)) result)
     (diagram-viewer-jobs-test--complete (car diagram-viewer-jobs-test--runs))
     (diagram-viewer--drain-events)
     (let ((second (diagram-viewer-submit source :callback (lambda (x) (setq result x)))))
       (should (eq 'succeeded (diagram-viewer-request-state second)))
       (should-not result)
       (let ((path (diagram-viewer--cache-path (diagram-viewer-request-cache-key first))))
         (delete-file path))
       (diagram-viewer--drain-events)
       (should (diagram-viewer-result-cache-hit-p result))
       (should (equal "fixture:one" (diagram-viewer-result-artifact-data result)))
       (should (= 1 (length diagram-viewer-jobs-test--runs)))))))

(ert-deftest diagram-viewer-jobs-cancel-one-preserves-other ()
  (diagram-viewer-jobs-test--with-env
   (let* ((a (diagram-viewer-submit source))
          (b (diagram-viewer-submit source)))
     (diagram-viewer-cancel a)
     (diagram-viewer-jobs-test--complete (car diagram-viewer-jobs-test--runs))
     (diagram-viewer--drain-events)
     (should (eq 'cancelled (diagram-viewer-request-state a)))
     (should (eq 'succeeded (diagram-viewer-request-state b))))))

(ert-deftest diagram-viewer-jobs-cancel-last-without-cancel-keeps-slot ()
  (diagram-viewer-jobs-test--with-env
   (diagram-viewer-cancel (diagram-viewer-submit source))
   (should (= 1 diagram-viewer--active-count))
   (let ((next (diagram-viewer-submit source)))
     (should (= 1 (length diagram-viewer-jobs-test--runs)))
     (diagram-viewer-jobs-test--complete (car diagram-viewer-jobs-test--runs))
     (diagram-viewer--drain-events)
     (should (eq 'succeeded (diagram-viewer-request-state next))))))

(ert-deftest diagram-viewer-jobs-cancel-stopping-key-blocks-only-that-key ()
  (diagram-viewer-jobs-test--with-env
   (diagram-viewer-register-renderer (diagram-viewer-jobs-test--renderer #'ignore) t)
   (let* ((old (diagram-viewer-submit source))
          (old-run (car diagram-viewer-jobs-test--runs)))
     (diagram-viewer-cancel old)
     (let ((new (diagram-viewer-submit source)))
       (should (eq 'pending (diagram-viewer-request-state new)))
       (should (= 1 (length diagram-viewer-jobs-test--runs)))
       (funcall (plist-get old-run :done) '(:status cancelled))
       (diagram-viewer--drain-events)
       (should (= 2 (length diagram-viewer-jobs-test--runs)))
       (should (eq 'running (diagram-viewer-request-state new)))
       (funcall (plist-get old-run :done) '(:status failed))
       (should (= 1 (hash-table-count diagram-viewer--jobs)))
       (diagram-viewer-jobs-test--complete (cadr diagram-viewer-jobs-test--runs))
       (diagram-viewer--drain-events)
       (should (eq 'succeeded (diagram-viewer-request-state new)))))))

(ert-deftest diagram-viewer-jobs-synchronous-cancel-retires-once ()
  (diagram-viewer-jobs-test--with-env
   (diagram-viewer-register-renderer
    (diagram-viewer-jobs-test--renderer
     (lambda (handle) (funcall (plist-get handle :done) '(:status cancelled)))) t)
   (let ((request (diagram-viewer-submit source)))
     (diagram-viewer-cancel request)
     (diagram-viewer-cancel request)
     (diagram-viewer--drain-events)
     (should (= 0 diagram-viewer--active-count))
     (should (= 0 (hash-table-count diagram-viewer--stopping-jobs)))
     (should (eq 'cancelled (diagram-viewer-request-state request))))))

(ert-deftest diagram-viewer-jobs-synchronous-render-is-delivered-later ()
  (diagram-viewer-jobs-test--with-env
   (diagram-viewer-register-renderer (diagram-viewer-jobs-test--renderer nil t) t)
   (let* (result
          (request (diagram-viewer-submit source :callback (lambda (x) (setq result x)))))
     (should (eq 'succeeded (diagram-viewer-request-state request)))
     (should-not result)
     (diagram-viewer--drain-events)
     (should result)
     (should (= 0 diagram-viewer--active-count)))))

(ert-deftest diagram-viewer-jobs-callback-error-is-isolated ()
  (diagram-viewer-jobs-test--with-env
   (let (other)
     (diagram-viewer-submit source :callback (lambda (_) (error "Callback failed")))
     (diagram-viewer-submit source :callback (lambda (x) (setq other x)))
     (diagram-viewer-jobs-test--complete (car diagram-viewer-jobs-test--runs))
     (diagram-viewer--drain-events)
     (should other)
     (should diagram-viewer--diagnostics)
     (should (= 0 (hash-table-count diagram-viewer--requests))))))

(ert-deftest diagram-viewer-jobs-source-edit-is-stale ()
  (diagram-viewer-jobs-test--with-env
   (let (result)
     (diagram-viewer-submit source :callback (lambda (x) (setq result x)))
     (goto-char (point-min))
     (insert "changed")
     (diagram-viewer-jobs-test--complete (car diagram-viewer-jobs-test--runs))
     (diagram-viewer--drain-events)
     (should (diagram-viewer-result-stale-p result)))))

(ert-deftest diagram-viewer-jobs-source-kill-cancels-request ()
  (diagram-viewer-jobs-test--with-env
   (let ((buffer (generate-new-buffer " *diagram-source*")) request)
     (unwind-protect
         (with-current-buffer buffer
           (insert "source")
           (setq request
                 (diagram-viewer-submit
                  (diagram-viewer-source-create
                   :text "source" :language 'mermaid :buffer buffer
                   :range (cons (copy-marker 1) (copy-marker (point-max) t)))))
           (kill-buffer buffer)
           (should (eq 'cancelled (diagram-viewer-request-state request)))
           (diagram-viewer-jobs-test--complete (car diagram-viewer-jobs-test--runs))
           (diagram-viewer--drain-events))
       (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest diagram-viewer-jobs-limit-and-fifo ()
  (diagram-viewer-jobs-test--with-env
   (let* ((a (diagram-viewer-submit source))
          (b (progn (setf (diagram-viewer-source-text source) "second")
                    (diagram-viewer-submit source)))
          (c (progn (setf (diagram-viewer-source-text source) "third")
                    (diagram-viewer-submit source))))
     (should (= 2 (length diagram-viewer-jobs-test--runs)))
     (should (eq 'pending (diagram-viewer-request-state c)))
     (diagram-viewer-jobs-test--complete (car diagram-viewer-jobs-test--runs))
     (diagram-viewer--drain-events)
     (should (= 3 (length diagram-viewer-jobs-test--runs)))
     (should (eq 'running (diagram-viewer-request-state c)))
     (diagram-viewer-jobs-test--complete (cadr diagram-viewer-jobs-test--runs))
     (diagram-viewer-jobs-test--complete (nth 2 diagram-viewer-jobs-test--runs))
     (diagram-viewer--drain-events)
     (should (cl-every (lambda (r) (eq 'succeeded (diagram-viewer-request-state r))) (list a b c))) )))

(ert-deftest diagram-viewer-jobs-queued-cancellation-does-not-start ()
  (diagram-viewer-jobs-test--with-env
   (let ((diagram-viewer-max-concurrent-jobs 1))
     (diagram-viewer-submit source)
     (setf (diagram-viewer-source-text source) "queued")
     (let ((queued (diagram-viewer-submit source)))
       (diagram-viewer-cancel queued)
       (diagram-viewer-jobs-test--complete (car diagram-viewer-jobs-test--runs))
       (diagram-viewer--drain-events)
       (should (= 1 (length diagram-viewer-jobs-test--runs)))
       (should-not diagram-viewer--queue)))))

(ert-deftest diagram-viewer-jobs-bypass-shares-job-but-not-cache ()
  (diagram-viewer-jobs-test--with-env
   (diagram-viewer-submit source)
   (diagram-viewer-jobs-test--complete (car diagram-viewer-jobs-test--runs))
   (diagram-viewer--drain-events)
   (let ((a (diagram-viewer-submit source :bypass-cache t))
         (b (diagram-viewer-submit source :bypass-cache t))
         (c (diagram-viewer-submit source)))
     (should (= 2 (length diagram-viewer-jobs-test--runs)))
     (should (eq 'succeeded (diagram-viewer-request-state c)))
     (diagram-viewer-jobs-test--complete (cadr diagram-viewer-jobs-test--runs) "fixture:new")
     (diagram-viewer--drain-events)
     (should (eq 'succeeded (diagram-viewer-request-state a)))
     (should (eq 'succeeded (diagram-viewer-request-state b))))))

(ert-deftest diagram-viewer-jobs-failed-refresh-preserves-cache ()
  (diagram-viewer-jobs-test--with-env
   (let* ((request (diagram-viewer-submit source))
          (path (diagram-viewer--cache-path (diagram-viewer-request-cache-key request))))
     (diagram-viewer-jobs-test--complete (car diagram-viewer-jobs-test--runs))
     (diagram-viewer--drain-events)
     (let ((refresh (diagram-viewer-submit source :bypass-cache t)))
       (diagram-viewer-jobs-test--complete (cadr diagram-viewer-jobs-test--runs) "invalid")
       (diagram-viewer--drain-events)
       (should (eq 'failed (diagram-viewer-request-state refresh)))
       (should (equal "fixture:one" (diagram-viewer-jobs-test--validate path)))))))

(ert-deftest diagram-viewer-jobs-availability-is-before-cache ()
  (diagram-viewer-jobs-test--with-env
   (diagram-viewer-submit source)
   (diagram-viewer-jobs-test--complete (car diagram-viewer-jobs-test--runs))
   (diagram-viewer--drain-events)
   (let ((renderer (diagram-viewer-get-renderer 'fake)))
     (setf (diagram-viewer-renderer-available-p renderer)
           (lambda () '(:condition diagram-viewer-renderer-unavailable :message "Missing")))
     (diagram-viewer-register-renderer renderer t)
     (let ((request (diagram-viewer-submit source)))
       (should (eq 'failed (diagram-viewer-request-state request)))
       (diagram-viewer--drain-events)
       (should (= 1 (length diagram-viewer-jobs-test--runs)))))))

(ert-deftest diagram-viewer-jobs-start-error-frees-slot ()
  (diagram-viewer-jobs-test--with-env
   (let ((renderer (diagram-viewer-get-renderer 'fake)))
     (setf (diagram-viewer-renderer-render-function renderer) (lambda (&rest _) (error "Start failed")))
     (diagram-viewer-register-renderer renderer t)
     (let ((request (diagram-viewer-submit source)))
       (diagram-viewer--drain-events)
       (should (eq 'failed (diagram-viewer-request-state request)))
       (should (= 0 diagram-viewer--active-count))
       (should-not (directory-files diagram-viewer-cache-directory nil "\\`\\.diagram-viewer-"))))))

(ert-deftest diagram-viewer-jobs-malformed-and-spontaneous-cancel ()
  (diagram-viewer-jobs-test--with-env
   (let ((a (diagram-viewer-submit source)))
     (funcall (plist-get (car diagram-viewer-jobs-test--runs) :done) "bad")
     (diagram-viewer--drain-events)
     (should (eq 'failed (diagram-viewer-request-state a))))
   (let ((b (diagram-viewer-submit source)))
     (funcall (plist-get (cadr diagram-viewer-jobs-test--runs) :done) '(:status cancelled))
     (diagram-viewer--drain-events)
     (should (eq 'cancelled (diagram-viewer-request-state b))))))

(ert-deftest diagram-viewer-jobs-snapshot-options-and-renderer ()
  (diagram-viewer-jobs-test--with-env
   (setf (diagram-viewer-source-options source) (list :theme (copy-sequence "dark")))
   (let ((request (diagram-viewer-submit source)))
     (aset (plist-get (diagram-viewer-source-options source) :theme) 0 ?X)
     (diagram-viewer-unregister-renderer 'fake)
     (should (equal "dark" (plist-get (diagram-viewer-request-options request) :theme)))
     (diagram-viewer-jobs-test--complete (car diagram-viewer-jobs-test--runs))
     (diagram-viewer--drain-events)
     (should (eq 'succeeded (diagram-viewer-request-state request))))))

(ert-deftest diagram-viewer-jobs-canonical-values ()
  (should (equal (diagram-viewer--canonical-plist '(:a 1 :b "two"))
                 (diagram-viewer--canonical-plist '(:b "two" :a 1))))
  (should-not (equal (diagram-viewer--canonical "x") (diagram-viewer--canonical 'x)))
  (should-not (equal (diagram-viewer--canonical "x") (diagram-viewer--canonical "x\n")))
  (should (equal (diagram-viewer--canonical '(:map :a 1 :b 2))
                 (diagram-viewer--canonical '(:map :b 2 :a 1))))
  (dolist (bad (list 1.5 [1 2] (cons 1 2) (current-buffer)))
    (should-error (diagram-viewer--canonical bad) :type 'diagram-viewer-invalid-argument))
  (should-error (diagram-viewer--canonical-plist '(:a 1 :a 2))
                :type 'diagram-viewer-invalid-argument)
  (let ((cyclic (list 'a)))
    (setcdr cyclic cyclic)
    (should-error (diagram-viewer--canonical cyclic) :type 'diagram-viewer-invalid-argument)))

(ert-deftest diagram-viewer-jobs-production-validator-fails-closed ()
  ;; Do not use the fixture validator here.
  (should-error (diagram-viewer--validate-output "/not/read/by/placeholder.svg")
                :type 'diagram-viewer-invalid-output))

(ert-deftest diagram-viewer-jobs-setup-reentry-cannot-resurrect-request ()
  (dolist (stage '(availability context))
    (diagram-viewer-jobs-test--with-env
     (let ((buffer (generate-new-buffer " *reentrant-source*")) result request)
       (unwind-protect
           (progn
             (with-current-buffer buffer
               (insert "body")
               (let ((renderer (diagram-viewer-get-renderer 'fake))
                     (source (diagram-viewer-source-create
                              :text "body" :language 'mermaid :buffer buffer
                              :range (cons (copy-marker 1) (copy-marker 5 t)))))
                 (cl-flet ((cancel-source ()
                             (kill-buffer buffer)
                             ;; Simulate an adapter that enters the event loop.
                             (diagram-viewer--drain-events)
                             (should-not result)))
			  (if (eq stage 'availability)
                              (setf (diagram-viewer-renderer-available-p renderer)
				    (lambda () (cancel-source) t))
			    (setf (diagram-viewer-renderer-cache-context-function renderer)
				  (lambda (_) (cancel-source) '(:context (:version 1)))))
			  (diagram-viewer-register-renderer renderer t)
			  (setq request (diagram-viewer-submit source :callback (lambda (x) (setq result x)))))))
             (should (eq 'cancelled (diagram-viewer-request-state request)))
             (should-not diagram-viewer-jobs-test--runs)
             (should-not result)
             (diagram-viewer--drain-events)
             (should (eq 'cancelled (diagram-viewer-result-state result)))
             (should (= 0 (hash-table-count diagram-viewer--requests))))
         (when (buffer-live-p buffer) (kill-buffer buffer)))))))

(ert-deftest diagram-viewer-jobs-callback-quit-is-isolated ()
  (diagram-viewer-jobs-test--with-env
   (let (other)
     (diagram-viewer-submit source :callback (lambda (x) (setq other x)))
     (diagram-viewer-submit source :callback (lambda (_) (signal 'quit nil)))
     (diagram-viewer-jobs-test--complete (car diagram-viewer-jobs-test--runs))
     (diagram-viewer--drain-events)
     (should other)
     (should-not diagram-viewer--events)
     (should (= 0 (hash-table-count diagram-viewer--requests))))))

(ert-deftest diagram-viewer-jobs-canonical-rejects-functions ()
  (dolist (function (list '(lambda () t) (let ((captured 1)) (lambda () captured))))
    (should-error (diagram-viewer--canonical function)
                  :type 'diagram-viewer-invalid-argument)))

(ert-deftest diagram-viewer-jobs-option-order-is-not-key-order ()
  (diagram-viewer-jobs-test--with-env
   (setf (diagram-viewer-source-options source) '(:a 1 :b 2))
   (let ((a (diagram-viewer-submit source)))
     (setf (diagram-viewer-source-options source) '(:b 2 :a 1))
     (let ((b (diagram-viewer-submit source)))
       (should (equal (diagram-viewer-request-cache-key a) (diagram-viewer-request-cache-key b)))
       (should (= 1 (length diagram-viewer-jobs-test--runs)))
       (diagram-viewer-jobs-test--complete (car diagram-viewer-jobs-test--runs))
       (diagram-viewer--drain-events)))))

(ert-deftest diagram-viewer-jobs-rename-error-preserves-old-bytes ()
  (diagram-viewer-jobs-test--with-env
   (let ((initial (diagram-viewer-submit source)))
     (diagram-viewer-jobs-test--complete (car diagram-viewer-jobs-test--runs))
     (diagram-viewer--drain-events)
     (let ((refresh (diagram-viewer-submit source :bypass-cache t)))
       (cl-letf (((symbol-function 'rename-file)
                  (lambda (&rest _) (signal 'file-error '("Cannot rename")))))
		(diagram-viewer-jobs-test--complete (cadr diagram-viewer-jobs-test--runs) "fixture:new"))
       (diagram-viewer--drain-events)
       (should (eq 'failed (diagram-viewer-request-state refresh)))
       (should (equal "fixture:one"
                      (diagram-viewer-jobs-test--validate
                       (diagram-viewer--cache-path (diagram-viewer-request-cache-key initial)))))
       (should (= 0 diagram-viewer--active-count))))))

(ert-deftest diagram-viewer-jobs-cancel-during-render-before-handle ()
  (diagram-viewer-jobs-test--with-env
   (let ((renderer (diagram-viewer-get-renderer 'fake)) done cancelled)
     (setf (diagram-viewer-renderer-render-function renderer)
           (lambda (request _output complete)
             (setq done complete)
             (diagram-viewer-cancel request)
             'handle)
           (diagram-viewer-renderer-cancel-function renderer)
           (lambda (handle)
             (should (eq 'handle handle))
             (setq cancelled t)
             (funcall done '(:status cancelled))))
     (diagram-viewer-register-renderer renderer t)
     (let ((request (diagram-viewer-submit source)))
       (should cancelled)
       (should (eq 'cancelled (diagram-viewer-request-state request)))
       (diagram-viewer--drain-events)
       (should (= 0 diagram-viewer--active-count))))))

(ert-deftest diagram-viewer-jobs-freshness-quit-still-releases-request ()
  (diagram-viewer-jobs-test--with-env
   (setf (diagram-viewer-source-valid-p source) (lambda (_) (signal 'quit nil)))
   (let (result)
     (diagram-viewer-submit source :callback (lambda (x) (setq result x)))
     (diagram-viewer-jobs-test--complete (car diagram-viewer-jobs-test--runs))
     (diagram-viewer--drain-events)
     (should (diagram-viewer-result-stale-p result))
     (should (= 0 (hash-table-count diagram-viewer--requests))))))

(provide 'diagram-viewer-jobs-test)
;;; diagram-viewer-jobs-test.el ends here
