;;; diagram-viewer-commands-test.el --- Public command contracts -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise command ownership, diagnostic history and cache deletion boundaries.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'diagram-viewer)
(defvar diagram-viewer--viewer-cache-key)

(defmacro diagram-viewer-commands-test--with-env (&rest body)
  "Run BODY with fresh registries, diagnostics, source and cache directory."
  (declare (indent 0) (debug t))
  `(let ((diagram-viewer--renderers (make-hash-table :test #'eq))
         (diagram-viewer--source-adapters (make-hash-table :test #'eq))
         (diagram-viewer--jobs (make-hash-table :test #'equal))
         (diagram-viewer--stopping-jobs (make-hash-table :test #'equal))
         (diagram-viewer--requests (make-hash-table :test #'eql))
         (diagram-viewer--active-count 0) (diagram-viewer--queue nil)
         (diagram-viewer--events nil) (diagram-viewer--event-timer nil)
         (diagram-viewer--diagnostics nil) (diagram-viewer--next-diagnostic-id 0)
         (diagram-viewer-default-renderer-alist nil)
         (diagram-viewer-cache-directory (make-temp-file "diagram-viewer-command-test-" t)))
     (unwind-protect
         (with-temp-buffer
           (insert "source")
           ,@body)
       (when (timerp diagram-viewer--event-timer) (cancel-timer diagram-viewer--event-timer))
       (unless (string-prefix-p "diagram-viewer-command-test-"
                                (file-name-nondirectory diagram-viewer-cache-directory))
         (error "Unexpected cleanup path"))
       (delete-directory diagram-viewer-cache-directory t))))

(defun diagram-viewer-commands-test--source ()
  "Return an owned source descriptor in the current buffer."
  (diagram-viewer-source-create
   :text (buffer-string) :language 'mermaid :adapter-id 'test
   :buffer (current-buffer) :range (cons (copy-marker 1) (copy-marker (point-max) t))))

(ert-deftest diagram-viewer-command-transfers-a-copy-and-releases-extracted-source ()
  (diagram-viewer-commands-test--with-env
   (let ((source (diagram-viewer-commands-test--source)) viewer-copy)
     (cl-letf (((symbol-function 'diagram-viewer-source-at-point) (lambda (&rest _) source))
               ((symbol-function 'diagram-viewer-view)
                (lambda (descriptor &rest _)
                  (setq viewer-copy (diagram-viewer--copy-source descriptor))
                  'viewer-buffer)))
       (should (eq 'viewer-buffer (diagram-viewer-view-at-point))))
     (should-not (marker-buffer (car (diagram-viewer-source-range source))))
     (should (marker-buffer (car (diagram-viewer-source-range viewer-copy))))
     (diagram-viewer--release-source viewer-copy))))

(ert-deftest diagram-viewer-command-releases-source-on-view-failure ()
  (diagram-viewer-commands-test--with-env
   (let ((source (diagram-viewer-commands-test--source)))
     (cl-letf (((symbol-function 'diagram-viewer-source-at-point) (lambda (&rest _) source))
               ((symbol-function 'diagram-viewer-view) (lambda (&rest _) (error "Viewer failed"))))
       (should-error (diagram-viewer-view-at-point)))
     (should-not (marker-buffer (car (diagram-viewer-source-range source)))))))

(ert-deftest diagram-viewer-command-noninteractive-source-error-is-structured ()
  (diagram-viewer-commands-test--with-env
   (should-error (diagram-viewer-view-at-point) :type 'diagram-viewer-source-not-found)))

(ert-deftest diagram-viewer-command-interactive-error-is-user-friendly ()
  (diagram-viewer-commands-test--with-env
   (cl-letf (((symbol-function 'called-interactively-p) (lambda (&rest _) t)))
     (should-error (diagram-viewer-view-at-point) :type 'user-error))))

(ert-deftest diagram-viewer-command-default-prompts-only-for-ties ()
  (diagram-viewer-commands-test--with-env
   (let ((source (diagram-viewer-commands-test--source)) chosen)
     (cl-letf (((symbol-function 'called-interactively-p) (lambda (&rest _) t))
               ((symbol-function 'diagram-viewer-source-at-point)
                (lambda (_id choose) (setq chosen choose) source))
               ((symbol-function 'diagram-viewer-view) (lambda (&rest _) 'viewer)))
       (diagram-viewer-view-at-point)
       (should (eq chosen 'on-ambiguity))))))

(ert-deftest diagram-viewer-command-prefix-requests-explicit-source-choice ()
  (diagram-viewer-commands-test--with-env
   (let ((source (diagram-viewer-commands-test--source)) chosen)
     (cl-letf (((symbol-function 'diagram-viewer-source-at-point)
                (lambda (_id choose) (setq chosen choose) source))
               ((symbol-function 'diagram-viewer-view) (lambda (&rest _) 'viewer)))
       (diagram-viewer-view-at-point '(4))
       (should (eq chosen t))))))

(ert-deftest diagram-viewer-command-on-ambiguity-keeps-higher-priority ()
  (diagram-viewer-commands-test--with-env
   (let ((source (diagram-viewer-commands-test--source)))
     (dolist (entry '((low . 1) (high . 10)))
       (diagram-viewer-register-source-adapter
        (diagram-viewer-source-adapter-create
         :id (car entry) :priority (cdr entry)
         :applicable-p (lambda () t) :source-function (lambda () source))))
     (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (ert-fail "Unnecessary prompt"))))
       (should (eq source (diagram-viewer-source-at-point nil 'on-ambiguity))))
     (diagram-viewer--release-source source))))

(ert-deftest diagram-viewer-diagnostic-tails-are-bounded-and-independent ()
  (let ((diagram-viewer--diagnostics nil) (diagram-viewer--next-diagnostic-id 0)
        (diagram-viewer-log-tail-length 4))
    (let* ((message (copy-sequence "abcdefgh"))
           (data (diagram-viewer--record-diagnostic
                  (list :condition 'diagram-viewer-process-failed :message message
                        :stderr "12345678" :stdout "ABCDEFGH" :reason 'exit :exit-code 1))))
      (should (= 1 (plist-get data :id)))
      (should (equal "efgh" (plist-get data :message)))
      (should (equal "5678" (plist-get data :stderr)))
      (aset message 7 ?X)
      (should (equal "efgh" (plist-get data :message)))
      (should (eq data (car diagram-viewer--diagnostics))))))

(ert-deftest diagram-viewer-diagnostic-history-is-limited ()
  (let ((diagram-viewer--diagnostics nil) (diagram-viewer--next-diagnostic-id 0))
    (dotimes (_ 25) (diagram-viewer--diagnose '(error "Failure")))
    (should (= 20 (length diagram-viewer--diagnostics)))
    (should (= 25 (plist-get (car diagram-viewer--diagnostics) :id)))
    (should (= 6 (plist-get (car (last diagram-viewer--diagnostics)) :id)))))

(ert-deftest diagram-viewer-diagnostic-rejects-malformed-adapter-error-data ()
  (let ((diagram-viewer--diagnostics nil))
    (dolist (data '("wrong" (:message) (:message (buffer)) nil))
      (let ((record (diagram-viewer--record-diagnostic data)))
        (should (integerp (plist-get record :id)))
        (should (stringp (plist-get record :message)))))))

(ert-deftest diagram-viewer-cache-clear-is-narrow-and-nonrecursive ()
  (diagram-viewer-commands-test--with-env
   (let* ((key (make-string 64 ?a))
          (path (diagram-viewer--cache-path key))
          (other (expand-file-name "keep.svg" diagram-viewer-cache-directory))
          (nested (expand-file-name (concat (make-string 64 ?b) ".svg") diagram-viewer-cache-directory)))
     (write-region "cache" nil path nil 'silent)
     (write-region "other" nil other nil 'silent)
     (make-directory nested)
     (should (= 1 (diagram-viewer-cache-clear)))
     (should-not (file-exists-p path))
     (should (file-exists-p other))
     (should (file-directory-p nested)))))

(ert-deftest diagram-viewer-cache-clear-unlinks-only-the-cache-symlink ()
  (diagram-viewer-commands-test--with-env
   (let* ((target (expand-file-name "outside.svg" diagram-viewer-cache-directory))
          (link (diagram-viewer--cache-path (make-string 64 ?a))))
     (write-region "untouched" nil target nil 'silent)
     (make-symbolic-link target link)
     (should (= 1 (diagram-viewer-cache-clear)))
     (should-not (file-symlink-p link))
     (should (file-exists-p target)))))

(ert-deftest diagram-viewer-cache-clear-current-only ()
  (diagram-viewer-commands-test--with-env
   (let* ((diagram-viewer--viewer-cache-key (make-string 64 ?a))
          (current (diagram-viewer--cache-path diagram-viewer--viewer-cache-key))
          (other (diagram-viewer--cache-path (make-string 64 ?b))))
     (write-region "current" nil current nil 'silent)
     (write-region "other" nil other nil 'silent)
     (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t)))
       (should (= 1 (diagram-viewer-cache-clear t))))
     (should (file-exists-p other))
     (should-not (file-exists-p current)))))

(ert-deftest diagram-viewer-cache-clear-current-needs-a-viewer ()
  (diagram-viewer-commands-test--with-env
   (should-error (diagram-viewer-cache-clear t) :type 'user-error)))

(ert-deftest diagram-viewer-cache-clear-does-not-touch-running-jobs ()
  (diagram-viewer-commands-test--with-env
   (let ((key (make-string 64 ?a)))
     (puthash key 'running-job diagram-viewer--jobs)
     (write-region "cache" nil (diagram-viewer--cache-path key) nil 'silent)
     (should (= 1 (diagram-viewer-cache-clear)))
     (should (eq 'running-job (gethash key diagram-viewer--jobs))))))

(ert-deftest diagram-viewer-cache-clear-rejects-directory-symlink ()
  (diagram-viewer-commands-test--with-env
   (let* ((link (expand-file-name "cache-link" diagram-viewer-cache-directory))
          (target diagram-viewer-cache-directory))
     (make-symbolic-link target link)
     (dolist (directory (list link (file-name-as-directory link)))
       (let ((diagram-viewer-cache-directory directory))
         (should-error (diagram-viewer-cache-clear) :type 'diagram-viewer-storage-error))))))

(provide 'diagram-viewer-commands-test)
;;; diagram-viewer-commands-test.el ends here
