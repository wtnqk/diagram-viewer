;;; diagram-viewer-md-mode-test.el --- md-mode adapter tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Fake Core/viewer boundaries only; no browser or PNG renderer is started.
;; Run this file with ERT in batch mode and the project on load-path.
;; Real dependency contract tests are explicitly opt-in.  Set
;; DIAGRAM_VIEWER_REAL_MD_MODE=1 and add the real md-mode and textui repos
;; to load-path.  Missing opt-in/dependencies produce an ERT skip.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'diagram-viewer-adapter-md-mode)

(define-derived-mode diagram-viewer-md-test-mode fundamental-mode "DV-MD-Test"
  "Provide a synthetic md-mode descendant without loading md-mode.")
(put 'diagram-viewer-md-test-mode 'derived-mode-parent 'md-mode)

(defmacro diagram-viewer-md-test--buffer (text &rest body)
  "Insert TEXT in a synthetic md-mode buffer and run BODY at its start."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (diagram-viewer-md-test-mode)
     (insert ,text)
     (goto-char (point-min))
     ,@body))

(defun diagram-viewer-md-test--placeholder (&optional fence file)
  "Insert one real-layout fake placeholder using FENCE and FILE.
Return the start position."
  (let ((start (point)))
    (insert "\n \n\n")
    (add-text-properties
     start (+ start 4)
     (list 'md-render-source (or fence "```mermaid\ngraph TD\n```\n")
           'md-render-frozen t))
    (put-text-property (1+ start) (+ start 2) 'md-render-media-file
                       (or file "/not-created/diagram.png"))
    start))

(defun diagram-viewer-md-test--rendered-source (&optional position)
  "Return a rendered descriptor at POSITION or point."
  (let ((info (diagram-viewer-md-mode--rendered-info (or position (point)))))
    (should info)
    (diagram-viewer-md-mode--descriptor info 'rendered)))

(defun diagram-viewer-md-test--state ()
  "Return a snapshot of source buffer state relevant to noninterference."
  (list (save-restriction
          (widen)
          (buffer-substring (point-min) (point-max)))
        (buffer-modified-p) (copy-tree buffer-undo-list)
        (point) (point-min) (point-max) (overlay-lists)))

;; Each fixture is a distinct ERT test so failures identify the syntax case.
(dolist
    (fixture
     '((backtick "```mermaid\ngraph TD\n```\n" "graph TD\n")
       (tilde "~~~mermaid\nA-->B\n~~~~\n" "A-->B\n")
       (long-opening "````mermaid\na\n```\nb\n`````\n" "a\n```\nb\n")
       (wrong-closer "```mermaid\na\n~~~\nb\n```\n" "a\n~~~\nb\n")
       (trailing-closer-text "```mermaid\na\n``` nope\nb\n```\n" "a\n``` nope\nb\n")
       (info-token "``` mermaid title\na\n```\n" "a\n")
       (three-space "   ~~~mermaid\na\n   ~~~ \t\n" "a\n")
       (crlf "```mermaid\r\na\r\n```\r\n" "a\r\n")
       (no-final-newline "```mermaid\na\n```" "a\n")
       (blank-body "```mermaid\n\n```\n" "\n")
       (incomplete "```mermaid\na\n" nil)
       (empty "```mermaid\n```\n" nil)
       (uppercase "```Mermaid\na\n```\n" nil)
       (language-prefix "```mermaid-extra\na\n```\n" nil)
       (backtick-info "```mermaid `bad`\na\n```\n" nil)
       (other-language "```python\n```mermaid\na\n```\n```\n" nil)
       (nested-other-tilde "~~~~python\n```mermaid\na\n```\n~~~~\n" nil)
       (indent-four "    ```mermaid\n    a\n    ```\n" nil)
       (tab-indent "\t```mermaid\na\n```\n" nil)
       (list-two "- item\n  ```mermaid\n  a\n  ```\n" nil)
       (list-three "1. item\n   ```mermaid\n   a\n   ```\n" nil)
       (list-blank "- item\n\n  ```mermaid\n  a\n  ```\n" nil)
       (nested-list "- outer\n  - inner\n\n   ```mermaid\na\n   ```\n" nil)
       (list-lazy "- item\nlazy continuation\n  ```mermaid\na\n  ```\n" nil)
       (quote "> ```mermaid\n> a\n> ```\n" nil)
       (quote-lazy "> paragraph\n  ```mermaid\na\n  ```\n" nil)
       (list-empty-marker "-\n  ```mermaid\na\n  ```\n" nil)
       (ordered-empty-marker "1.\n   ```mermaid\na\n   ```\n" nil)
       (front-matter "---\n```mermaid\na\n```\n---\n" nil)
       (link-title "[ref]: /url \"title\n```mermaid\na\n```\n\"\n" nil)
       (html "<script>\n\n```mermaid\na\n```\n" nil)))
  (let ((name (nth 0 fixture)) (text (nth 1 fixture)) (expected (nth 2 fixture)))
    (eval
     `(ert-deftest ,(intern (format "diagram-viewer-md-test-fence-%s" name)) ()
        (diagram-viewer-md-test--buffer ,text
          (let ((info (diagram-viewer-md-mode--scan
                       ;; Container fixtures point at their apparent Mermaid.
                       (save-excursion
                         (search-forward "mermaid" nil t)
                         (point)))))
            (should (equal (plist-get info :text) ,expected)))))
     t)))

(ert-deftest diagram-viewer-md-test-point-boundaries ()
  (diagram-viewer-md-test--buffer "before\n```mermaid\na\n```\nafter\n"
    (let ((start 8) (end 25))
      ;; Derive offsets from the actual closing fence, not body assumptions.
      (goto-char start)
      (let ((info (diagram-viewer-md-mode--edit-info (point))))
        (setq end (plist-get info :end))
        (should-not (diagram-viewer-md-mode--edit-info (1- start)))
        (cl-loop for pos from start below end do
                 (should (diagram-viewer-md-mode--edit-info pos)))
        (should-not (diagram-viewer-md-mode--edit-info end))))))

(ert-deftest diagram-viewer-md-test-point-max-no-newline ()
  (diagram-viewer-md-test--buffer "```mermaid\na\n```"
    (should (diagram-viewer-md-mode--edit-info (point-max)))))

(ert-deftest diagram-viewer-md-test-leave-list-at-column-zero ()
  (diagram-viewer-md-test--buffer "- item\n\n```mermaid\na\n```\n"
    (search-forward "mermaid")
    (should (diagram-viewer-md-mode--edit-info (point)))))

(ert-deftest diagram-viewer-md-test-edit-rejects-rendered-code-panel ()
  (dolist (property '(md-render-frozen md-render-source-block-body md-render-source))
    (diagram-viewer-md-test--buffer "```mermaid\ngraph TD\n```\n"
      ;; Rendering an outer Python fence removes its delimiters, leaving
      ;; these literal inner fence lines.  One rendered-body character is
      ;; enough to prove the otherwise complete candidate is not editable.
      (put-text-property 12 13 property t)
      (should (diagram-viewer-md-mode--scan (point)))
      (should-not (diagram-viewer-md-mode--edit-info (point)))
      (should-not (diagram-viewer-md-mode--applicable-p))
      (should-error (diagram-viewer-md-mode--source-at-point)
                    :type 'diagram-viewer-source-not-found))))

(ert-deftest diagram-viewer-md-test-standalone-rejects-surrounding-text ()
  (dolist (text '("before\n```mermaid\na\n```\n"
                  "```mermaid\na\n```\nafter"
                  "```mermaid\na\n```\n\n"
                  "```mermaid\na\n```\n```mermaid\nb\n```\n"))
    (should-not (diagram-viewer-md-mode--standalone text))))

(ert-deftest diagram-viewer-md-test-mode-and-registry ()
  (let ((diagram-viewer--source-adapters (make-hash-table :test #'eq)))
    (with-temp-buffer
      (insert "```mermaid\na\n```\n")
      (goto-char 1)
      (should (eq (diagram-viewer-md-mode-enable) 'md-mode))
      (should-error (diagram-viewer-md-mode-enable)
                    :type 'diagram-viewer-invalid-argument)
      (should (eq (diagram-viewer-md-mode-enable t) 'md-mode))
      (should-not (diagram-viewer-md-mode--applicable-p))
      (should-error (diagram-viewer-source-at-point 'md-mode)
                    :type 'diagram-viewer-source-not-found)
      (diagram-viewer-md-test-mode)
      (let ((source (diagram-viewer-source-at-point 'md-mode)))
        (unwind-protect
            (progn
              (should (eq (diagram-viewer-source-origin-adapter-id source) 'md-mode))
              (should (equal (diagram-viewer-source-text source) "a\n"))
              (should-not (marker-insertion-type (car (diagram-viewer-source-range source))))
              (should (marker-insertion-type (cdr (diagram-viewer-source-range source))))
              (should (equal (diagram-viewer--range-text source)
                             "```mermaid\na\n```\n")))
          (diagram-viewer--release-source source))))))

(ert-deftest diagram-viewer-md-test-enable-no-buffer-effects ()
  (diagram-viewer-md-test--buffer "text\n"
    (buffer-enable-undo)
    (set-buffer-modified-p nil)
    (let ((diagram-viewer--source-adapters (make-hash-table :test #'eq))
          (before (diagram-viewer-md-test--state))
          (locals (buffer-local-variables)))
      (cl-letf (((symbol-function 'add-hook) (lambda (&rest _) (ert-fail "Hook added")))
                ((symbol-function 'make-overlay) (lambda (&rest _) (ert-fail "Overlay created"))))
        (diagram-viewer-md-mode-enable))
      (should (equal before (diagram-viewer-md-test--state)))
      (should (equal locals (buffer-local-variables))))))

(ert-deftest diagram-viewer-md-test-edit-copy-release-refresh ()
  (diagram-viewer-md-test--buffer "```mermaid\na\n```\n"
    (let* ((original (diagram-viewer-md-mode--source-at-point))
           (copy (diagram-viewer--copy-source original)))
      (unwind-protect
          (progn
            (diagram-viewer--release-source original)
            (should (funcall (diagram-viewer-source-valid-p copy) copy))
            (goto-char 12)
            (insert "new-")
            (should-not (funcall (diagram-viewer-source-valid-p copy) copy))
            (let ((fresh (funcall (diagram-viewer-source-refresh-function copy) copy)))
              (unwind-protect
                  (should (equal (diagram-viewer-source-text fresh) "new-a\n"))
                (diagram-viewer--release-source fresh))))
        (diagram-viewer--release-source original)
        (diagram-viewer--release-source copy)))))

(ert-deftest diagram-viewer-md-test-unrelated-edit-stays-current ()
  (diagram-viewer-md-test--buffer "```mermaid\na\n```\n\noutside"
    (let* ((source (diagram-viewer-md-mode--source-at-point))
           (snapshot (diagram-viewer--range-text source)))
      (unwind-protect
          (progn (goto-char (point-max)) (insert " changed")
                 (should (diagram-viewer--source-current-p source snapshot)))
        (diagram-viewer--release-source source)))))

(ert-deftest diagram-viewer-md-test-refresh-missing-never-reuses-body ()
  (diagram-viewer-md-test--buffer "```mermaid\na\n```\n"
    (let ((source (diagram-viewer-md-mode--source-at-point)))
      (unwind-protect
          (progn (erase-buffer) (insert "not a diagram")
                 (should-not (diagram-viewer-md-mode--valid-p source))
                 (should-error (diagram-viewer-md-mode--refresh source)
                               :type 'diagram-viewer-source-not-found))
        (diagram-viewer--release-source source)))))

(ert-deftest diagram-viewer-md-test-rendered-all-four-positions ()
  (diagram-viewer-md-test--buffer ""
    (let ((start (diagram-viewer-md-test--placeholder)))
      (dotimes (offset 4)
        (let ((source (diagram-viewer-md-test--rendered-source (+ start offset))))
          (unwind-protect
              (progn
                (should (= (car (diagram-viewer-source-range source)) start))
                (should (= (cdr (diagram-viewer-source-range source)) (+ start 4)))
                (should (equal (diagram-viewer-source-text source) "graph TD\n")))
            (diagram-viewer--release-source source))))
      (should-not (diagram-viewer-md-mode--rendered-info (+ start 4))))))

(ert-deftest diagram-viewer-md-test-rendered-equal-source-adjacent ()
  (diagram-viewer-md-test--buffer ""
    (diagram-viewer-md-test--placeholder)
    (diagram-viewer-md-test--placeholder)
    (dotimes (offset 8)
      (let ((info (diagram-viewer-md-mode--rendered-info (1+ offset))))
        (should (= (plist-get info :start) (if (< offset 4) 1 5)))
        (should (= (plist-get info :end) (if (< offset 4) 5 9)))))))

(ert-deftest diagram-viewer-md-test-rendered-rejects-invalid-contract ()
  (dolist (mutation
           (list (lambda () (put-text-property 2 3 'md-render-media-file "relative.png"))
                 (lambda () (put-text-property 2 3 'md-render-media-file "/image.svg"))
                 (lambda () (put-text-property 2 3 'md-render-media-file nil))
                 (lambda () (put-text-property 1 2 'md-render-media-file "/image.png"))
                 (lambda () (put-text-property 3 4 'md-render-media-file "/image.png"))
                 (lambda () (put-text-property 1 2 'md-render-source "different"))
                 (lambda () (put-text-property 4 5 'md-render-frozen nil))
                 (lambda () (put-text-property 1 5 'md-render-source
                                               "```python\na\n```\n"))
                 (lambda () (put-text-property 1 5 'md-render-source
                                               "```mermaid\na\n"))))
    (diagram-viewer-md-test--buffer ""
      (diagram-viewer-md-test--placeholder)
      (funcall mutation)
      (dotimes (offset 4)
        (should-not (diagram-viewer-md-mode--rendered-info (1+ offset)))))))

(ert-deftest diagram-viewer-md-test-rendered-command-noninterference ()
  (diagram-viewer-md-test--buffer "prefix\n"
    (goto-char (point-max))
    (let ((start (diagram-viewer-md-test--placeholder)) captured)
      (insert "suffix\n")
      (put-text-property (1+ start) (+ start 2) 'display '(image :type png :file "/fake.png"))
      (buffer-enable-undo)
      (setq buffer-undo-list '((7 . 8)))
      (set-buffer-modified-p nil)
      (narrow-to-region (1+ start) (+ start 3))
      (goto-char (point-min))
      (let ((before (diagram-viewer-md-test--state)))
        (cl-letf (((symbol-function 'diagram-viewer-view)
                   (lambda (source)
                     (setq captured (diagram-viewer--copy-source source))
                     'fake-viewer)))
          (should (eq (diagram-viewer-md-mode-view-rendered-at-point) 'fake-viewer)))
        (unwind-protect
            (progn (should (equal before (diagram-viewer-md-test--state)))
                   (should (equal (diagram-viewer-source-text captured) "graph TD\n"))
                   (should (diagram-viewer-md-mode--valid-p captured)))
          (diagram-viewer--release-source captured))))))

(ert-deftest diagram-viewer-md-test-rendered-target-error-interface ()
  (diagram-viewer-md-test--buffer "not a placeholder"
    (should-error (diagram-viewer-md-mode-view-rendered-at-point)
                  :type 'diagram-viewer-source-not-found)
    (cl-letf (((symbol-function 'called-interactively-p) (lambda (&rest _) t)))
      (should-error (diagram-viewer-md-mode-view-rendered-at-point)
                    :type 'user-error))))

(ert-deftest diagram-viewer-md-test-rendered-command-release-on-error ()
  (diagram-viewer-md-test--buffer ""
    (diagram-viewer-md-test--placeholder)
    (goto-char 2)
    (let (captured)
      (cl-letf (((symbol-function 'diagram-viewer-view)
                 (lambda (source) (setq captured source) (error "Fake failure"))))
        (should-error (diagram-viewer-md-mode-view-rendered-at-point)))
      (should-not (marker-buffer (car (diagram-viewer-source-range captured)))))))

(ert-deftest diagram-viewer-md-test-rendered-current-properties-refresh ()
  (diagram-viewer-md-test--buffer ""
    (diagram-viewer-md-test--placeholder)
    (let* ((original (diagram-viewer-md-test--rendered-source 2))
           (copy (diagram-viewer--copy-source original)))
      (unwind-protect
          (progn
            (diagram-viewer--release-source original)
            (put-text-property 2 3 'display '(image :type png :data "new image"))
            (should (diagram-viewer-md-mode--valid-p copy))
            (put-text-property 1 5 'md-render-source "~~~mermaid\nnew body\n~~~\n")
            (put-text-property 2 3 'md-render-media-file "/new.png")
            (should-not (diagram-viewer-md-mode--valid-p copy))
            (let ((fresh (diagram-viewer-md-mode--refresh copy)))
              (unwind-protect
                  (progn
                    (should (equal (diagram-viewer-source-text fresh) "new body\n"))
                    (should (equal (plist-get (diagram-viewer-source-adapter-data fresh) :media)
                                   "/new.png")))
                (diagram-viewer--release-source fresh))))
        (diagram-viewer--release-source original)
        (diagram-viewer--release-source copy)))))

(ert-deftest diagram-viewer-md-test-view-switch-invalidates-old-descriptors ()
  (diagram-viewer-md-test--buffer ""
    (diagram-viewer-md-test--placeholder)
    (let ((source (diagram-viewer-md-test--rendered-source 2)))
      (unwind-protect
          (progn
            (erase-buffer) (insert "```mermaid\nnew body\n```\n")
            (should-not (diagram-viewer-md-mode--valid-p source))
            (should-error (diagram-viewer-md-mode--refresh source)
                          :type 'diagram-viewer-source-not-found))
        (diagram-viewer--release-source source)))))

(ert-deftest diagram-viewer-md-test-rendered-reconstruction-never-revives ()
  (diagram-viewer-md-test--buffer ""
    (diagram-viewer-md-test--placeholder)
    (let* ((original (diagram-viewer-md-test--rendered-source 2))
           (source (diagram-viewer--copy-source original))
           (tick (plist-get (diagram-viewer-source-adapter-data source) :chars-tick)))
      (unwind-protect
          (progn
            (diagram-viewer--release-source original)
            (erase-buffer)
            (insert "```mermaid\ngraph TD\n```\n")
            (erase-buffer)
            (diagram-viewer-md-test--placeholder)
            ;; Identical text, properties and expanded marker range must not
            ;; revive a descriptor whose original placeholder disappeared.
            (should (equal (diagram-viewer--range-text source) "\n \n\n"))
            (should (diagram-viewer-md-mode--rendered-info 2))
            (should-not (= tick (buffer-chars-modified-tick)))
            (should-not (diagram-viewer-md-mode--valid-p source))
            (should-error (diagram-viewer-md-mode--refresh source)
                          :type 'diagram-viewer-source-not-found))
        (diagram-viewer--release-source original)
        (diagram-viewer--release-source source)))))

(ert-deftest diagram-viewer-md-test-rendered-png-properties-preserve-epoch ()
  (diagram-viewer-md-test--buffer ""
    (diagram-viewer-md-test--placeholder)
    (let* ((source (diagram-viewer-md-test--rendered-source 2))
           (tick (buffer-chars-modified-tick)))
      (unwind-protect
          (progn
            (with-silent-modifications
              (put-text-property 2 3 'display '(image :type png :data "PNG completed")))
            (should (= tick (buffer-chars-modified-tick)))
            (should (diagram-viewer-md-mode--valid-p source))
            (let ((fresh (diagram-viewer-md-mode--refresh source)))
              (unwind-protect
                  (should (diagram-viewer-md-mode--valid-p fresh))
                (diagram-viewer--release-source fresh))))
        (diagram-viewer--release-source source)))))

(ert-deftest diagram-viewer-md-test-rendered-outside-character-edit-ends-epoch ()
  (diagram-viewer-md-test--buffer ""
    (diagram-viewer-md-test--placeholder)
    (insert "outside\n")
    (let ((source (diagram-viewer-md-test--rendered-source 2)))
      (unwind-protect
          (progn
            (goto-char (point-max))
            (insert "changed\n")
            ;; Rendered character edits end the epoch even when the owned
            ;; placeholder range and its saved properties still match.
            (should (equal (diagram-viewer--range-text source) "\n \n\n"))
            (should-not (diagram-viewer-md-mode--valid-p source))
            (should-error (diagram-viewer-md-mode--refresh source)
                          :type 'diagram-viewer-source-not-found))
        (diagram-viewer--release-source source)))))

(ert-deftest diagram-viewer-md-test-dead-buffer-and-mode-change ()
  (let (source)
    (diagram-viewer-md-test--buffer "```mermaid\na\n```\n"
      (setq source (diagram-viewer-md-mode--source-at-point))
      (fundamental-mode)
      (should-not (diagram-viewer-md-mode--valid-p source))
      (should-error (diagram-viewer-md-mode--refresh source)
                    :type 'diagram-viewer-source-not-found))
    (unwind-protect
        (progn (should-not (diagram-viewer-md-mode--valid-p source))
               (should-error (diagram-viewer-md-mode--refresh source)
                             :type 'diagram-viewer-source-not-found))
      (diagram-viewer--release-source source))))

(declare-function md-mode "md-mode" ())
(declare-function md-render--insert-media "md-render-media" (&rest args))
(declare-function md-mode-show-source "md-mode" ())

(ert-deftest diagram-viewer-md-test-realmd-placeholder-contract ()
  "Test actual md-mode placeholder creation without starting any renderer."
  (skip-unless (getenv "DIAGRAM_VIEWER_REAL_MD_MODE"))
  (skip-unless (and (locate-library "md-mode") (locate-library "textui")))
  (require 'md-mode)
  (with-temp-buffer
    (md-mode)
    (insert "```mermaid\ngraph TD\n```\n")
    (let* ((fence (buffer-string))
           (start (point-min)) (end (point-max)) source)
      (cl-letf (((symbol-function 'md-render--media-cache-file)
                 (lambda (&rest _) "/not-created/realmd.png"))
                ((symbol-function 'md-render--watch-media) #'ignore))
        (md-render--insert-media :start start :end end :source fence
                                 :render-source "graph TD" :backend 'mermaid
                                 :block-p t :label "Mermaid"))
      (setq source (diagram-viewer-md-test--rendered-source 2))
      (unwind-protect
          (progn
            (should (equal (buffer-substring-no-properties 1 5) "\n \n\n"))
            (should (equal (diagram-viewer-source-text source) "graph TD\n"))
            (let ((before (diagram-viewer-md-test--state)))
              (dotimes (offset 4)
                (should (diagram-viewer-md-mode--rendered-info (1+ offset))))
              (should (equal before (diagram-viewer-md-test--state))))
            ;; The real public source-view command reconstructs Markdown;
            ;; the adapter must not resurrect the former placeholder.
            (setq-local md-mode--rendered-p t)
            (md-mode-show-source)
            (should (equal (buffer-string) fence))
            (should-not (diagram-viewer-md-mode--valid-p source))
            (should-error (diagram-viewer-md-mode--refresh source)
                          :type 'diagram-viewer-source-not-found))
        (diagram-viewer--release-source source)))))

;;; diagram-viewer-md-mode-test.el ends here
