;;; diagram-viewer-test.el --- Core contract tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with: emacs -Q --batch -L . -l test/diagram-viewer-test.el \
;;             -f ert-run-tests-batch-and-exit
;; These tests need neither a renderer executable nor a source-mode adapter.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'diagram-viewer)

(defmacro diagram-viewer-test--with-registry (&rest body)
  "Run BODY with independent Core registries and renderer preferences."
  (declare (indent 0) (debug t))
  `(let ((diagram-viewer--renderers (make-hash-table :test #'eq))
         (diagram-viewer--source-adapters (make-hash-table :test #'eq))
         (diagram-viewer-default-renderer-alist nil))
     ,@body))

(defun diagram-viewer-test--renderer (&optional id)
  "Return a valid fake renderer with ID, defaulting to `fake'."
  (diagram-viewer-renderer-create
   :id (or id 'fake) :languages (list 'mermaid) :output-extension ".svg"
   :available-p (lambda () t)
   :cache-context-function (lambda (_) '(:context (:version 1)))
   :render-function (lambda (&rest _) nil)))

(defun diagram-viewer-test--source (&optional start end)
  "Return an owned source for current buffer between START and END."
  (let ((start (or start (point-min))) (end (or end (point-max))))
    (diagram-viewer-source-create
     :text (buffer-substring-no-properties start end)
     :language 'mermaid :adapter-id 'fake-source :buffer (current-buffer)
     :range (cons (copy-marker start nil) (copy-marker end t))
     :options (list :theme (copy-sequence "dark"))
     :adapter-data (list :fence (copy-sequence "original")))))

(defun diagram-viewer-test--adapter (id priority source-function)
  "Return an applicable adapter with ID, PRIORITY and SOURCE-FUNCTION."
  (diagram-viewer-source-adapter-create
   :id id :priority priority :applicable-p (lambda () t)
   :source-function source-function))

(ert-deftest diagram-viewer-test-public-keyword-constructors ()
  "All five public types expose keyword constructors and accessors."
  (with-temp-buffer
    (insert "graph TD\n  A-->B\n")
    (let* ((source (diagram-viewer-test--source))
           (renderer (diagram-viewer-test--renderer))
           (adapter (diagram-viewer-test--adapter 'fake-source 10 (lambda () source)))
           (request (diagram-viewer-request-create
                     :id 1 :source "graph TD\n" :language 'mermaid
                     :source-adapter-id 'fake-source :source-buffer (current-buffer)
                     :source-range (diagram-viewer-source-range source)
                     :options '(:theme "dark") :state 'pending))
           (result (diagram-viewer-result-create
                    :request-id 1 :state 'failed :error '(:message "test"))))
      (unwind-protect
          (progn
            (should (diagram-viewer-source-p source))
            (should (diagram-viewer-renderer-p renderer))
            (should (diagram-viewer-source-adapter-p adapter))
            (should (diagram-viewer-request-p request))
            (should (diagram-viewer-result-p result))
            (should (eq (diagram-viewer-source-language source) 'mermaid))
            (should (equal (diagram-viewer-source-text source) "graph TD\n  A-->B\n"))
            (should (eq (diagram-viewer-request-state request) 'pending))
            (should (= (diagram-viewer-result-request-id result) 1)))
        (diagram-viewer--release-source source)))))

(ert-deftest diagram-viewer-test-renderer-registry-lifecycle-and-copies ()
  "Registration and lookup protect descriptors; replacement is explicit."
  (diagram-viewer-test--with-registry
    (let ((original (diagram-viewer-test--renderer)))
      (diagram-viewer-register-renderer original)
      (should-error (diagram-viewer-register-renderer original)
                    :type 'diagram-viewer-invalid-argument)
      (setcar (diagram-viewer-renderer-languages original) 'other)
      (let ((retrieved (diagram-viewer-get-renderer 'fake)))
        (should-not (eq original retrieved))
        (should (equal (diagram-viewer-renderer-languages retrieved) '(mermaid)))
        (setcar (diagram-viewer-renderer-languages retrieved) 'changed))
      (should (equal (diagram-viewer-renderer-languages
                      (diagram-viewer-get-renderer 'fake)) '(mermaid)))
      (let ((replacement (diagram-viewer-test--renderer)))
        (setf (diagram-viewer-renderer-languages replacement) '(dot))
        (diagram-viewer-register-renderer replacement t))
      (should (equal (diagram-viewer-renderer-languages
                      (diagram-viewer-get-renderer 'fake)) '(dot))))
    (diagram-viewer-unregister-renderer 'fake)
    (should-not (diagram-viewer-get-renderer 'fake))))

(ert-deftest diagram-viewer-test-source-adapter-registry-lifecycle-and-copies ()
  "Source adapter registration, lookup, replacement and removal are isolated."
  (diagram-viewer-test--with-registry
    (let ((adapter (diagram-viewer-test--adapter 'fake-source 10 (lambda () nil))))
      (diagram-viewer-register-source-adapter adapter)
      (should-error (diagram-viewer-register-source-adapter adapter)
                    :type 'diagram-viewer-invalid-argument)
      (setf (diagram-viewer-source-adapter-priority adapter) 99)
      (let ((retrieved (diagram-viewer-get-source-adapter 'fake-source)))
        (should-not (eq adapter retrieved))
        (should (= (diagram-viewer-source-adapter-priority retrieved) 10))
        (setf (diagram-viewer-source-adapter-priority retrieved) 50))
      (should (= (diagram-viewer-source-adapter-priority
                  (diagram-viewer-get-source-adapter 'fake-source)) 10))
      (diagram-viewer-register-source-adapter adapter t)
      (should (= (diagram-viewer-source-adapter-priority
                  (diagram-viewer-get-source-adapter 'fake-source)) 99)))
    (diagram-viewer-unregister-source-adapter 'fake-source)
    (should-not (diagram-viewer-get-source-adapter 'fake-source))))

(ert-deftest diagram-viewer-test-register-rejects-invalid-renderers ()
  "Renderer fields are checked at registration, not just construction."
  (diagram-viewer-test--with-registry
    (should-error (diagram-viewer-register-renderer '(not a renderer))
                  :type 'diagram-viewer-invalid-argument)
    (dolist (field '(:id :languages :output-extension :available-p
                    :cache-context-function :render-function :cancel-function))
      (let ((renderer (diagram-viewer-test--renderer)))
        (cl-ecase field
          (:id (setf (diagram-viewer-renderer-id renderer) "fake"))
          (:languages (setf (diagram-viewer-renderer-languages renderer) '("mermaid")))
          (:output-extension (setf (diagram-viewer-renderer-output-extension renderer) "svg"))
          (:available-p (setf (diagram-viewer-renderer-available-p renderer) 42))
          (:cache-context-function
           (setf (diagram-viewer-renderer-cache-context-function renderer) 42))
          (:render-function (setf (diagram-viewer-renderer-render-function renderer) nil))
          (:cancel-function (setf (diagram-viewer-renderer-cancel-function renderer) 42)))
        (ert-info ((format "Invalid renderer field: %s" field))
          (should-error (diagram-viewer-register-renderer renderer)
                        :type 'diagram-viewer-invalid-argument))))))

(ert-deftest diagram-viewer-test-register-rejects-invalid-source-adapters ()
  "Source adapter fields are checked by the public registration API."
  (diagram-viewer-test--with-registry
    (should-error (diagram-viewer-register-source-adapter 'not-an-adapter)
                  :type 'diagram-viewer-invalid-argument)
    (dolist (field '(:id :priority :applicable-p :source-function :describe-function))
      (let ((adapter (diagram-viewer-test--adapter 'fake-source 10 (lambda () nil))))
        (cl-ecase field
          (:id (setf (diagram-viewer-source-adapter-id adapter) "fake-source"))
          (:priority (setf (diagram-viewer-source-adapter-priority adapter) "high"))
          (:applicable-p (setf (diagram-viewer-source-adapter-applicable-p adapter) nil))
          (:source-function (setf (diagram-viewer-source-adapter-source-function adapter) 42))
          (:describe-function (setf (diagram-viewer-source-adapter-describe-function adapter) 42)))
        (ert-info ((format "Invalid adapter field: %s" field))
          (should-error (diagram-viewer-register-source-adapter adapter)
                        :type 'diagram-viewer-invalid-argument))))))

(ert-deftest diagram-viewer-test-submit-rejects-invalid-call-contract ()
  "Submit rejects source and callback type errors synchronously."
  (diagram-viewer-test--with-registry
    (should-error (diagram-viewer-submit "not a source")
                  :type 'diagram-viewer-invalid-argument)
    (with-temp-buffer
      (insert "graph TD\n")
      (let ((source (diagram-viewer-test--source)))
        (unwind-protect
            (progn
              (should-error (diagram-viewer-submit source :callback 42)
                            :type 'diagram-viewer-invalid-argument)
              (setf (diagram-viewer-source-text source) 42)
              (should-error (diagram-viewer-submit source)
                            :type 'diagram-viewer-invalid-argument))
          (diagram-viewer--release-source source))))))

(ert-deftest diagram-viewer-test-renderer-explicit-and-unique-selection ()
  "A sole renderer is selected; explicit IDs must support the language."
  (diagram-viewer-test--with-registry
    (diagram-viewer-register-renderer (diagram-viewer-test--renderer))
    (should (eq (diagram-viewer-renderer-id
                 (diagram-viewer-select-renderer 'mermaid)) 'fake))
    (should (eq (diagram-viewer-renderer-id
                 (diagram-viewer-select-renderer 'mermaid 'fake)) 'fake))
    (should-error (diagram-viewer-select-renderer 'dot 'fake)
                  :type 'diagram-viewer-renderer-not-found)
    (should-error (diagram-viewer-select-renderer 'mermaid 'missing)
                  :type 'diagram-viewer-renderer-not-found)))

(ert-deftest diagram-viewer-test-renderer-default-is-authoritative ()
  "A configured default wins, but invalid defaults never silently fall back."
  (diagram-viewer-test--with-registry
    (diagram-viewer-register-renderer (diagram-viewer-test--renderer 'first))
    (diagram-viewer-register-renderer (diagram-viewer-test--renderer 'second))
    (setq diagram-viewer-default-renderer-alist '((mermaid . second)))
    (should (eq (diagram-viewer-renderer-id
                 (diagram-viewer-select-renderer 'mermaid)) 'second))
    (should (eq (diagram-viewer-renderer-id
                 (diagram-viewer-select-renderer 'mermaid 'first)) 'first))
    (setq diagram-viewer-default-renderer-alist '((mermaid . missing)))
    (should-error (diagram-viewer-select-renderer 'mermaid)
                  :type 'diagram-viewer-renderer-not-found)))

(ert-deftest diagram-viewer-test-renderer-ambiguity-never-prompts ()
  "Noninteractive renderer selection fails for zero or multiple candidates."
  (diagram-viewer-test--with-registry
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (ert-fail "Noninteractive renderer prompted"))))
      (should-error (diagram-viewer-select-renderer 'mermaid)
                    :type 'diagram-viewer-renderer-not-found)
      (diagram-viewer-register-renderer (diagram-viewer-test--renderer 'first))
      (diagram-viewer-register-renderer (diagram-viewer-test--renderer 'second))
      (should-error (diagram-viewer-select-renderer 'mermaid)
                    :type 'diagram-viewer-renderer-not-found))))

(ert-deftest diagram-viewer-test-source-priority-fallback-and-errors ()
  "Try applicable sources by priority, falling back only on source-not-found."
  (diagram-viewer-test--with-registry
    (with-temp-buffer
      (insert "graph TD\n")
      (let ((source (diagram-viewer-test--source)) calls)
        (unwind-protect
            (progn
              (diagram-viewer-register-source-adapter
               (diagram-viewer-test--adapter
                'low 1 (lambda () (push 'low calls) source)))
              (diagram-viewer-register-source-adapter
               (diagram-viewer-test--adapter
                'high 20 (lambda () (push 'high calls)
                           (signal 'diagram-viewer-source-not-found nil))))
              (should (diagram-viewer-source-p (diagram-viewer-source-at-point)))
              (should (equal (reverse calls) '(high low)))
              (diagram-viewer-register-source-adapter
               (diagram-viewer-test--adapter 'high 20 (lambda () (error "Extraction failed"))) t)
              (setq calls nil)
              (should-error (diagram-viewer-source-at-point) :type 'error)
              (should-not calls))
          (diagram-viewer--release-source source))))))

(ert-deftest diagram-viewer-test-source-ambiguity-and-explicit-choice ()
  "Equal priorities require choose or an explicit adapter, never hash order."
  (diagram-viewer-test--with-registry
    (with-temp-buffer
      (insert "graph TD\n")
      (let ((source (diagram-viewer-test--source)) selected)
        (unwind-protect
            (progn
              (dolist (id '(first second))
                (let ((adapter-id id))
                  (diagram-viewer-register-source-adapter
                   (diagram-viewer-test--adapter
                    id 10 (lambda () (setq selected adapter-id) source)))))
              (cl-letf (((symbol-function 'completing-read)
                         (lambda (&rest _) (ert-fail "Ambiguous source prompted without choose"))))
                (should-error (diagram-viewer-source-at-point)
                              :type 'diagram-viewer-source-not-found))
              (should-not selected)
              (should (diagram-viewer-source-p (diagram-viewer-source-at-point 'first)))
              (should (eq selected 'first))
              (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "second")))
                (should (diagram-viewer-source-p (diagram-viewer-source-at-point nil t))))
              (should (eq selected 'second)))
          (diagram-viewer--release-source source))))))

(ert-deftest diagram-viewer-test-source-copy-and-release-ownership ()
  "Copies own markers, strings and nested data; release is owner-local."
  (with-temp-buffer
    (insert (propertize "graph TD\n" 'face 'bold))
    (let* ((source (diagram-viewer-test--source))
           (copy (diagram-viewer--copy-source source))
           (range (diagram-viewer-source-range source))
           (copied-range (diagram-viewer-source-range copy)))
      (unwind-protect
          (progn
            (should-not (eq source copy))
            (should-not (eq (car range) (car copied-range)))
            (should-not (eq (cdr range) (cdr copied-range)))
            (should-not (marker-insertion-type (car copied-range)))
            (should (marker-insertion-type (cdr copied-range)))
            (should (equal (diagram-viewer-source-text copy) "graph TD\n"))
            (should-not (text-properties-at 0 (diagram-viewer-source-text copy)))
            (aset (diagram-viewer-source-text source) 0 ?X)
            (aset (plist-get (diagram-viewer-source-options source) :theme) 0 ?X)
            (aset (plist-get (diagram-viewer-source-adapter-data source) :fence) 0 ?X)
            (should (equal (diagram-viewer-source-text copy) "graph TD\n"))
            (should (equal (diagram-viewer-source-options copy) '(:theme "dark")))
            (should (equal (diagram-viewer-source-adapter-data copy) '(:fence "original")))
            (diagram-viewer--release-source copy)
            (should-not (marker-buffer (car copied-range)))
            (should-not (marker-buffer (cdr copied-range)))
            (should (eq (marker-buffer (car range)) (current-buffer))))
        (diagram-viewer--release-source copy)
        (diagram-viewer--release-source source)))))

(ert-deftest diagram-viewer-test-source-freshness-range-and-boundaries ()
  "Outside edits preserve freshness; inside and boundary insertions do not."
  (dolist (where '(outside inside start end))
    (with-temp-buffer
      (insert "before\nbody\nafter")
      (let ((source (diagram-viewer-test--source 8 12)))
        (unwind-protect
            (progn
              (should (diagram-viewer--source-current-p source "body"))
              (goto-char (cl-ecase where (outside 1) (inside 9) (start 8) (end 12)))
              (insert "X")
              (ert-info ((format "Insertion: %s" where))
                (if (eq where 'outside)
                    (should (diagram-viewer--source-current-p source "body"))
                  (should-not (diagram-viewer--source-current-p source "body")))))
          (diagram-viewer--release-source source))))))

(ert-deftest diagram-viewer-test-source-freshness-rejects-invalid-markers ()
  "Dead buffers, detached or foreign markers and reversed ranges are stale."
  (dolist (failure '(detached foreign reversed dead-buffer))
    (let ((buffer (generate-new-buffer " *diagram-viewer-source-test*"))
          (other (generate-new-buffer " *diagram-viewer-other-test*")) source)
      (unwind-protect
          (progn
            (with-current-buffer buffer
              (insert "body")
              (setq source (diagram-viewer-test--source)))
            (let ((range (diagram-viewer-source-range source)))
              (cl-ecase failure
                (detached (set-marker (car range) nil))
                (foreign (set-marker (cdr range) 1 other))
                (reversed (set-marker (car range) 5 buffer)
                          (set-marker (cdr range) 1 buffer))
                (dead-buffer (kill-buffer buffer))))
            (ert-info ((format "Invalid source: %s" failure))
              (should-not (diagram-viewer--source-current-p source "body"))))
        (when source (diagram-viewer--release-source source))
        (when (buffer-live-p buffer) (kill-buffer buffer))
        (kill-buffer other)))))

(ert-deftest diagram-viewer-test-source-valid-p-receives-owned-descriptor ()
  "Predicates receive the copy, run after common checks, and fail closed."
  (with-temp-buffer
    (insert "body")
    (let ((source (diagram-viewer-test--source)) copy seen)
      (unwind-protect
          (progn
            (setf (diagram-viewer-source-valid-p source)
                  (lambda (owned) (setq seen owned) t))
            (setq copy (diagram-viewer--copy-source source))
            (diagram-viewer--release-source source)
            (should (diagram-viewer--source-current-p copy "body"))
            (should (eq seen copy))
            (setq seen nil)
            (should-not (diagram-viewer--source-current-p copy "different"))
            (should-not seen)
            (setf (diagram-viewer-source-valid-p copy) (lambda (_) nil))
            (should-not (diagram-viewer--source-current-p copy "body"))
            (setf (diagram-viewer-source-valid-p copy)
                  (lambda (_) (error "Predicate failed")))
            (should-not (diagram-viewer--source-current-p copy "body")))
        (when copy (diagram-viewer--release-source copy))
        (diagram-viewer--release-source source)))))

(provide 'diagram-viewer-test)
;;; diagram-viewer-test.el ends here
