;;; check-docs.el --- Validate diagram-viewer design documents -*- lexical-binding: t; -*-

;;; Commentary:
;; Execute from the project root with emacs -Q --batch -l test/check-docs.el.

;;; Code:
(require 'org)
(require 'org-lint)

(let ((failures 0) (blocks 0) (links 0)
      (files (directory-files "docs" t "\\.org\\'")))
  (dolist (file files)
    (with-current-buffer (find-file-noselect file)
      (org-mode)
      (let ((issues (org-lint)))
        (princ (format "%s: org-lint=%S\n" (file-name-nondirectory file) issues))
        (when issues (cl-incf failures)))
      (let ((tree (org-element-parse-buffer)))
        (org-element-map tree 'link
			 (lambda (link)
			   (when (equal (org-element-property :type link) "file")
			     (let ((path (org-element-property :path link)))
			       ;; Absolute links are evidence in a local dependency checkout,
			       ;; not prerequisites for another machine to validate this repo.
			       (unless (file-name-absolute-p path)
				 (cl-incf links)
				 (unless (file-exists-p (expand-file-name path default-directory))
				   (cl-incf failures)
				   (princ (format "%s: broken relative link %s\n" file path))))))))
        (org-element-map tree 'src-block
			 (lambda (block)
			   (when (equal (org-element-property :language block) "emacs-lisp")
			     (cl-incf blocks)
			     (with-temp-buffer
			       (insert (org-element-property :value block))
			       (goto-char (point-min))
			       (condition-case err
				   (progn
				     (check-parens)
				     (while (progn (forward-comment (point-max)) (not (eobp)))
				       (read (current-buffer))))
				 (error
				  (cl-incf failures)
				  (princ (format "%s: reader error %S\n" file err)))))))))))
  (princ (format "%d documents, %d Lisp blocks, %d relative links; %d failures\n"
                 (length files) blocks links failures))
  (kill-emacs (if (zerop failures) 0 1)))

;;; check-docs.el ends here
