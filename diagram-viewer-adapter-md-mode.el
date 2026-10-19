;;; diagram-viewer-adapter-md-mode.el --- Read md-mode diagrams -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: diagram-viewer contributors
;; Version: 0.1.0
;; Keywords: multimedia, tools

;;; Commentary:

;; Register with `diagram-viewer-md-mode-enable'.  Edit view uses the Core
;; command `diagram-viewer-view-at-point'; rendered placeholders use
;; `diagram-viewer-md-mode-view-rendered-at-point'.  No hook, overlay, PNG
;; rendering, or md-mode internal function is used by this adapter.
;;
;; This is deliberately not a general Markdown parser.  List, blockquote,
;; tab/indented-code and ambiguous container contexts are rejected.  A blank
;; line followed by a column-zero line can leave a list/quote context; an
;; indented fence cannot.  HTML-like markup, link reference definitions,
;; and initial YAML front matter make the remaining document unknown:
;; their multiline content is intentionally not parsed by this adapter.
;; Rendered support is restricted to the four-character block placeholder
;; contract documented for md-mode 0.5.0.  Unknown layouts fail closed.

;;; Code:

(require 'cl-lib)
(require 'diagram-viewer)

(declare-function diagram-viewer-view "diagram-viewer" (source &rest args))

(defun diagram-viewer-md-mode--opening (line)
  "Return fence delimiter and info for an opening LINE, or nil."
  (let ((case-fold-search nil))
    (when (string-match "\\` \\{0,3\\}\\(`\\{3,\\}\\|~\\{3,\\}\\)\\(.*\\)\\'" line)
      (let ((delimiter (match-string 1 line)) (info (match-string 2 line)))
        (unless (and (eq (aref delimiter 0) ?`) (string-match-p "`" info))
          (list delimiter info))))))

(defun diagram-viewer-md-mode--closing-p (line delimiter)
  "Return non-nil if LINE closes a fence opened with DELIMITER."
  (let ((case-fold-search nil))
    (string-match-p
     (concat "\\` \\{0,3\\}" (regexp-quote (substring delimiter 0 1))
             "\\{" (number-to-string (length delimiter)) ",\\}[ \t\r]*\\'")
     line)))

(defun diagram-viewer-md-mode--mermaid-p (info)
  "Return non-nil if the first token of INFO is lowercase mermaid."
  (let ((case-fold-search nil))
    (string-match-p "\\`[ \t]*mermaid\\(?:[ \t\r]\\|\\'\\)" info)))

(defun diagram-viewer-md-mode--scan (position &optional standalone)
  "Return a complete Mermaid fence containing POSITION, or nil.
Scan the entire accessible buffer, including other-language fences.
STANDALONE bypasses the conservative container gate, for a single saved
rendered block only.  Return plain data, never markers.  Empty bodies,
list/quote contexts, four-space/tab indentation and unknown contexts are
unsupported.  A final closing line without newline includes `point-max'."
  (save-excursion
    (goto-char (point-min))
    (let ((context 'top-level) (blank t) open found)
      (while (and (not found) (< (point) (point-max)))
        (let* ((start (point))
               (line (buffer-substring-no-properties start (line-end-position)))
               (end (progn (forward-line 1) (point)))
               (opening (diagram-viewer-md-mode--opening line)))
          (cond
           (open
            (when (diagram-viewer-md-mode--closing-p line (nth 1 open))
              (when (and (nth 3 open)
                         (diagram-viewer-md-mode--mermaid-p (nth 2 open))
                         (< (nth 4 open) start)
                         (<= (car open) position)
                         (or (< position end)
                             (and (= position end) (= end (point-max))
                                  (not (eq (char-before end) ?\n)))))
                (setq found
                      (list :start (car open) :end end
                            :text (buffer-substring-no-properties (nth 4 open) start)
                            :fence (buffer-substring-no-properties (car open) end))))
              (setq open nil)))
           (t
            ;; A column-zero block after a blank cannot continue a list or
            ;; quote.  HTML is intentionally sticky: even blank lines may
            ;; occur inside a raw HTML block.
            (unless standalone
              (when (and blank (eq context 'container)
                         (string-match-p "\\`[^ \t>]" line))
                (setq context 'top-level))
              (cond
               ((or (string-match-p "\\`[ \t]*<" line)
                    (string-match-p "\\` \\{0,3\\}\\[[^]]+\\]:" line)
                    (and (= start (point-min))
                         (string-match-p "\\`---[ \t\r]*\\'" line)))
                (setq context 'unknown))
               ((and (not (eq context 'unknown))
                     (or (string-match-p "\\`[ \t]*>" line)
                         (string-match-p "\\` *\\(?:[-+*]\\|[0-9]+[.)]\\)\\(?:[ \t\r]+\\|\\'\\)" line)
                         (string-match-p "\\`\\(?:    \\| *\t\\)" line)))
                (setq context 'container))))
            (when opening
              ;; Even rejected fences consume their bodies, so their nested
              ;; Mermaid-looking lines can never become new opening fences.
              (setq open (list start (car opening) (cadr opening)
                               (or standalone (eq context 'top-level)) end)))))
          (setq blank (string-match-p "\\`[ \t\r]*\\'" line))))
      found)))

(defun diagram-viewer-md-mode--standalone (fence)
  "Return parsed body for a single complete FENCE string, or nil.
Reject leading/trailing text, empty bodies and non-Mermaid blocks."
  (when (stringp fence)
    (with-temp-buffer
      (insert (substring-no-properties fence))
      (let ((info (diagram-viewer-md-mode--scan (point-min) t)))
        (and info (= (plist-get info :start) (point-min))
             (= (plist-get info :end) (point-max))
             (plist-get info :text))))))

(defun diagram-viewer-md-mode--edit-info (position)
  "Return the current editable fence at POSITION, or nil.
Reject frozen/rendered source-block content: literal Mermaid fences in
another language's rendered code panel are not editable diagram sources."
  (when (derived-mode-p 'md-mode)
    (save-restriction
      (widen)
      (let ((info (diagram-viewer-md-mode--scan position)))
        (and info
             (not (cl-some
                   (lambda (property)
                     (text-property-not-all (plist-get info :start)
                                            (plist-get info :end) property nil))
                   '(md-render-frozen md-render-source-block-body md-render-source)))
             info)))))

(defun diagram-viewer-md-mode--rendered-info (position)
  "Return current placeholder data containing POSITION, or nil.
Require the exact md-mode 0.5.0 block layout: newline, image space,
newline, newline.  All four characters must share source and frozen
properties; only the image space may have a media-file property.
Adjacent equal-source placeholders remain separate.  The PNG need not
exist and its display property is not examined or changed."
  (when (derived-mode-p 'md-mode)
    (save-restriction
      (widen)
      (cl-loop for image from (- position 2) to (1+ position)
               for start = (1- image) for end = (+ image 3)
               when (and (<= (point-min) start) (<= end (point-max))
                         (<= start position) (< position end)
                         (equal (buffer-substring-no-properties start end) "\n \n\n"))
               thereis
               (let ((fence (get-text-property image 'md-render-source))
                     (file (get-text-property image 'md-render-media-file)))
                 (when (and (stringp file) (file-name-absolute-p file)
                            (not (file-remote-p file))
                            (string-suffix-p ".png" file t)
                            (cl-loop for at from start below end
                                     always
                                     (and (equal (get-text-property at 'md-render-source) fence)
                                          (eq (get-text-property at 'md-render-frozen) t)
                                          (or (= at image)
                                              (not (get-text-property at 'md-render-media-file)))))
                            (not (get-text-property (1- image) 'md-render-media-file))
                            (not (get-text-property (1+ image) 'md-render-media-file)))
                   (when-let* ((text (diagram-viewer-md-mode--standalone fence)))
                     (list :start start :end end :text text
                           :fence (substring-no-properties fence) :media file))))))))

(defun diagram-viewer-md-mode--descriptor (info kind)
  "Create an owned source from current buffer INFO of KIND."
  (diagram-viewer-source-create
   :text (plist-get info :text) :language 'mermaid :adapter-id 'md-mode
   :buffer (current-buffer)
   :range (cons (copy-marker (plist-get info :start) nil)
                (copy-marker (plist-get info :end) t))
   :valid-p #'diagram-viewer-md-mode--valid-p
   :refresh-function #'diagram-viewer-md-mode--refresh
   :adapter-data (list :kind kind :fence (plist-get info :fence)
                       :media (plist-get info :media)
                       :chars-tick (and (eq kind 'rendered)
                                        (buffer-chars-modified-tick)))))

(defun diagram-viewer-md-mode--owned-info (source)
  "Re-read current information at the live range owned by SOURCE.
Do not search other ranges if this one disappeared or changed views.
Rendered buffers are read-only: any character modification ends their
saved epoch, even if identical placeholders are reconstructed later.
Property-only PNG updates keep the epoch; Edit sources have no epoch gate."
  (let ((buffer (diagram-viewer-source-buffer source))
        (range (diagram-viewer-source-range source))
        (kind (plist-get (diagram-viewer-source-adapter-data source) :kind)))
    (when (and (buffer-live-p buffer) (consp range)
               (markerp (car range)) (markerp (cdr range))
               (eq (marker-buffer (car range)) buffer)
               (eq (marker-buffer (cdr range)) buffer)
               (< (marker-position (car range)) (marker-position (cdr range))))
      (with-current-buffer buffer
        (let ((info (pcase kind
                      ('edit (diagram-viewer-md-mode--edit-info (marker-position (car range))))
                      ('rendered
                       (and (eql (plist-get (diagram-viewer-source-adapter-data source)
                                            :chars-tick)
                                 (buffer-chars-modified-tick))
                            (diagram-viewer-md-mode--rendered-info
                             (marker-position (car range))))))))
          (and info (= (plist-get info :start) (marker-position (car range)))
               (= (plist-get info :end) (marker-position (cdr range))) info))))))

(defun diagram-viewer-md-mode--valid-p (source)
  "Return non-nil if SOURCE still denotes the same current input.
Use only SOURCE's owned markers, never captured extraction markers.
PNG display-only changes do not invalidate the descriptor."
  (when-let* ((info (diagram-viewer-md-mode--owned-info source)))
    (let ((data (diagram-viewer-source-adapter-data source)))
      (and (equal (plist-get info :fence) (plist-get data :fence))
           (equal (plist-get info :media) (plist-get data :media))
           (equal (plist-get info :text) (diagram-viewer-source-text source))))))

(defun diagram-viewer-md-mode--refresh (source)
  "Re-extract current input from the live range owned by SOURCE.
Signal `diagram-viewer-source-not-found' for deleted ranges, view or
mode changes, unsupported syntax and missing current properties.  Never
reuse the old body when re-extraction fails."
  (let ((info (diagram-viewer-md-mode--owned-info source)))
    (unless info (signal 'diagram-viewer-source-not-found nil))
    (with-current-buffer (diagram-viewer-source-buffer source)
      (diagram-viewer-md-mode--descriptor
       info (plist-get (diagram-viewer-source-adapter-data source) :kind)))))

(defun diagram-viewer-md-mode--applicable-p ()
  "Return non-nil for a supported editable Mermaid fence at point."
  (and (diagram-viewer-md-mode--edit-info (point)) t))

(defun diagram-viewer-md-mode--source-at-point ()
  "Extract an owned editable Mermaid source descriptor at point.
Support only complete top-level fences in `md-mode' derived buffers.
List, blockquote, indented-code and unknown container contexts are
rejected conservatively.  HTML-like markup, link definitions and initial
YAML front matter make the remaining buffer unknown and unsupported."
  (let ((info (diagram-viewer-md-mode--edit-info (point))))
    (unless info (signal 'diagram-viewer-source-not-found nil))
    (diagram-viewer-md-mode--descriptor info 'edit)))

;;;###autoload
(defun diagram-viewer-md-mode-enable (&optional replace)
  "Register the md-mode source adapter, allowing explicit REPLACE.
Only register ID `md-mode'; do not load md-mode, add hooks, decorate
buffers, create overlays or intervene in inline PNG rendering.
An already registered ID requires non-nil REPLACE, like the Core API."
  (interactive "P")
  (diagram-viewer-register-source-adapter
   (diagram-viewer-source-adapter-create
    :id 'md-mode :priority 10
    :applicable-p #'diagram-viewer-md-mode--applicable-p
    :source-function #'diagram-viewer-md-mode--source-at-point)
   replace))

;;;###autoload
(defun diagram-viewer-md-mode-view-rendered-at-point ()
  "Open the current md-mode Mermaid placeholder in the Core viewer.
Accept point on the image space or anywhere in its four-character
placeholder.  Re-read the current source and media-file properties and
require a single complete Mermaid fence.  Unsupported layouts signal
`diagram-viewer-source-not-found'.  Do not modify source buffer text,
properties, overlays, undo, modified state, point or narrowing.
Interactive target failures become a short `user-error'; Lisp calls keep
the structured `diagram-viewer-source-not-found' condition."
  (interactive)
  (condition-case err
      (let ((info (diagram-viewer-md-mode--rendered-info (point))))
        (unless info (signal 'diagram-viewer-source-not-found nil))
        (let ((source (diagram-viewer-md-mode--descriptor info 'rendered)))
          (unwind-protect
              (diagram-viewer-view source)
            (diagram-viewer--release-source source))))
    (diagram-viewer-source-not-found
     (if (called-interactively-p 'interactive)
         (user-error "No supported Mermaid placeholder at point")
       (signal (car err) (cdr err))))))

(provide 'diagram-viewer-adapter-md-mode)
;;; diagram-viewer-adapter-md-mode.el ends here
