;;; diagram-viewer-svg.el --- Restricted passive SVG validation -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: diagram-viewer contributors
;; Version: 0.1.0
;; Keywords: multimedia, tools

;;; Commentary:

;; Internal Core helper.  Accept a restricted, passive SVG subset, then emit
;; fresh UTF-8 XML from the checked tree.  Never pass the original input bytes
;; to the image decoder.  This is not a general-purpose SVG sanitizer or an OS
;; sandbox.  Unsupported XML, SVG and CSS constructs are rejected, not ignored.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'xml)
(require 'diagram-viewer)

(defcustom diagram-viewer-svg-max-bytes (* 20 1024 1024)
  "Maximum number of input bytes accepted for an SVG artifact."
  :type '(integer 1 *) :group 'diagram-viewer)
(defcustom diagram-viewer-svg-max-depth 64
  "Maximum XML element nesting depth accepted before parsing."
  :type '(integer 1 *) :group 'diagram-viewer)
(defcustom diagram-viewer-svg-max-nodes 100000
  "Maximum number of XML elements accepted before parsing."
  :type '(integer 1 *) :group 'diagram-viewer)
(defcustom diagram-viewer-svg-max-css-bytes (* 1024 1024)
  "Maximum CSS bytes accepted in one style element or attribute."
  :type '(integer 1 *) :group 'diagram-viewer)

(defcustom diagram-viewer-svg-max-css-tokens 100000
  "Maximum number of lexical tokens accepted in one CSS fragment."
  :type '(integer 1 *) :group 'diagram-viewer)

(defconst diagram-viewer-svg--max-number 16384
  "Maximum absolute numeric CSS or geometry component in this subset.")

(defconst diagram-viewer-svg-policy-version 1
  "Version of the accepted SVG subset, independent of renderer versions.")
(defconst diagram-viewer-svg--namespace "http://www.w3.org/2000/svg")
(defconst diagram-viewer-svg--xmlns "http://www.w3.org/2000/xmlns/")
(defconst diagram-viewer-svg--xml "http://www.w3.org/XML/1998/namespace")
(defconst diagram-viewer-svg--xlink "http://www.w3.org/1999/xlink")
(defconst diagram-viewer-svg--elements
  '("svg" "g" "defs" "style" "title" "desc" "path" "rect" "circle"
    "ellipse" "line" "polyline" "polygon" "text" "tspan" "marker"
    "linearGradient" "radialGradient" "stop" "clipPath" "symbol" "filter" "feDropShadow"))
(defconst diagram-viewer-svg--attributes
  '("id" "class" "role" "aria-roledescription" "aria-describedby" "aria-labelledby"
    "aria-label" "aria-hidden" "viewBox" "preserveAspectRatio" "version"
    "x" "y" "x1" "y1" "x2" "y2" "dx" "dy" "width" "height"
    "rx" "ry" "r" "cx" "cy" "d" "points" "pathLength" "transform"
    "transform-origin" "textLength" "lengthAdjust" "dominant-baseline"
    "alignment-baseline" "text-anchor" "markerWidth" "markerHeight" "refX" "refY"
    "orient" "markerUnits" "gradientUnits" "gradientTransform" "spreadMethod"
    "offset" "clipPathUnits" "type" "name" "stdDeviation" "filterUnits" "primitiveUnits"))
(defconst diagram-viewer-svg--css-properties
  '("fill" "fill-opacity" "fill-rule" "stroke" "stroke-width" "stroke-opacity"
    "stroke-linecap" "stroke-linejoin" "stroke-miterlimit" "stroke-dasharray"
    "stroke-dashoffset" "opacity" "color" "background" "background-color"
    "font" "font-family" "font-size" "font-weight" "font-style" "font-variant"
    "font-variant-numeric" "line-height" "text-align" "text-anchor"
    "text-decoration" "text-decoration-color" "text-transform" "text-shadow"
    "white-space" "word-break" "word-wrap" "overflow-wrap" "word-spacing"
    "letter-spacing" "dominant-baseline" "alignment-baseline" "vertical-align"
    "display" "visibility" "overflow" "cursor" "pointer-events"
    "width" "height" "min-width" "min-height" "max-width" "max-height"
    "margin" "margin-top" "margin-right" "margin-bottom" "margin-left"
    "padding" "padding-top" "padding-right" "padding-bottom" "padding-left"
    "border" "border-width" "border-style" "border-color" "border-radius"
    "box-sizing" "position" "top" "right" "bottom" "left" "z-index"
    "transform" "transform-origin" "transform-box" "clip-path"
    "marker-start" "marker-mid" "marker-end" "stop-color" "stop-opacity"
    "paint-order" "shape-rendering" "text-rendering" "vector-effect" "clip-rule"
    "filter" "box-shadow" "flood-color" "flood-opacity"
    "animation" "animation-name" "animation-duration" "animation-timing-function"
    "animation-iteration-count" "animation-direction" "animation-fill-mode"
    "animation-delay" "animation-play-state"))
(defconst diagram-viewer-svg--css-functions
  '("rgb" "rgba" "hsl" "hsla"
    "translate" "translatex" "translatey" "scale" "scalex" "scaley"
    "rotate" "skew" "skewx" "skewy" "matrix" "cubic-bezier" "steps" "drop-shadow"))

(defun diagram-viewer-svg--reject (message)
  "Reject an SVG with a source-free diagnostic MESSAGE."
  (signal 'diagram-viewer-invalid-output (list message)))

(defun diagram-viewer-svg--at-p (text position prefix)
  "Test PREFIX at POSITION in TEXT without copying a large suffix."
  (and (<= (+ position (length prefix)) (length text))
       (string= prefix (substring text position (+ position (length prefix))))))

(defun diagram-viewer-svg--xml-char-p (char)
  "Return non-nil for a valid XML 1.0 character CHAR."
  (or (memq char '(9 10 13)) (<= #x20 char #xd7ff)
      (<= #xe000 char #xfffd) (<= #x10000 char #x10ffff)))

(defun diagram-viewer-svg--escape (text)
  "Escape TEXT for XML text content and double-quoted attributes."
  (unless (cl-loop for char across text always (diagram-viewer-svg--xml-char-p char))
    (diagram-viewer-svg--reject "Invalid XML character"))
  (let ((result text))
    (dolist (pair '(("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;")
                    ("\"" . "&quot;") ("'" . "&apos;")))
      (setq result (string-replace (car pair) (cdr pair) result)))
    result))

(defun diagram-viewer-svg--preflight (text)
  "Bound XML TEXT before its strict namespace-aware parse."
  (let ((case-fold-search t) (pos 0) (depth 0) (nodes 0) (size (length text)))
    (when (string-match-p "<![ \t\r\n]*\\(?:DOCTYPE\\|ENTITY\\)" text)
      (diagram-viewer-svg--reject "DTD and entity declarations are forbidden"))
    (when (string-prefix-p "\ufeff" text) (setq pos 1))
    (when (diagram-viewer-svg--at-p text pos "<?xml ")
      (let ((end (string-match "?>" text pos)))
        (unless end (diagram-viewer-svg--reject "Unterminated XML declaration"))
        (let ((declaration (substring text pos (+ end 2))))
          (unless (string-match-p
                   "\\`<\\?xml[ \t\r\n]+version[ \t\r\n]*=[ \t\r\n]*[\"']1\\.0[\"']\\(?:[ \t\r\n]+encoding[ \t\r\n]*=[ \t\r\n]*[\"']UTF-8[\"']\\)?\\(?:[ \t\r\n]+standalone[ \t\r\n]*=[ \t\r\n]*[\"']\\(?:yes\\|no\\)[\"']\\)?[ \t\r\n]*\\?>\\'"
                   declaration)
            (diagram-viewer-svg--reject "Only XML 1.0 with UTF-8 encoding is accepted")))
        (setq pos (+ end 2))))
    (while (and (< pos size) (setq pos (string-match "<" text pos)))
      (cond
       ((diagram-viewer-svg--at-p text pos "<!--")
        (let ((end (string-match "-->" text (+ pos 4))))
          (unless end (diagram-viewer-svg--reject "Unterminated XML comment"))
          (setq pos (+ end 3))))
       ((diagram-viewer-svg--at-p text pos "<![CDATA[")
        (let ((end (string-match "]]>" text (+ pos 9))))
          (unless end (diagram-viewer-svg--reject "Unterminated CDATA section"))
          (setq pos (+ end 3))))
       (t
        (when (memq (and (< (1+ pos) size) (aref text (1+ pos))) '(?! ??))
          (diagram-viewer-svg--reject "XML declarations and processing instructions are forbidden"))
        (let ((i (1+ pos)) quote end)
          (while (and (< i size) (not end))
            (let ((char (aref text i)))
              (cond (quote (when (= char quote) (setq quote nil)))
                    ((memq char '(?\" ?')) (setq quote char))
                    ((= char ?>) (setq end i))))
            (setq i (1+ i)))
          (unless end (diagram-viewer-svg--reject "Unterminated XML element"))
          (if (= (aref text (1+ pos)) ?/)
              (setq depth (1- depth))
            (cl-incf nodes)
            (unless (= (aref text (1- end)) ?/) (cl-incf depth)))
          (when (or (< depth 0) (> depth diagram-viewer-svg-max-depth)
                    (> nodes diagram-viewer-svg-max-nodes))
            (diagram-viewer-svg--reject "SVG nesting or element count exceeds the policy limit"))
          (setq pos (1+ end))))))))

(defun diagram-viewer-svg--entities (text)
  "Reject undeclared or malformed entity references in XML TEXT."
  (with-temp-buffer
    (insert text) (goto-char (point-min))
    (while (re-search-forward "&\\|<!--\\|<!\\[CDATA\\[" nil t)
      (pcase (match-string-no-properties 0)
        ("<!--" (unless (search-forward "-->" nil t)
                  (diagram-viewer-svg--reject "Unterminated XML comment")))
        ("<![CDATA[" (unless (search-forward "]]>" nil t)
                       (diagram-viewer-svg--reject "Unterminated CDATA section")))
        (_ (unless (looking-at "\\(?:amp\\|lt\\|gt\\|apos\\|quot\\|#[0-9]+\\|#x[0-9A-Fa-f]+\\);")
             (diagram-viewer-svg--reject "Unknown or malformed XML entity reference"))
           (goto-char (match-end 0)))))))

(defun diagram-viewer-svg--css-tokens (css)
  "Tokenize restricted CSS using anchored, forward-only scans."
  (when (> (string-bytes css) diagram-viewer-svg-max-css-bytes)
    (diagram-viewer-svg--reject "CSS exceeds the policy size limit"))
  (with-temp-buffer
    (insert css)
    (goto-char (point-min))
    (let (tokens (count 0) (case-fold-search nil))
      (while (not (eobp))
        (when (> (cl-incf count) diagram-viewer-svg-max-css-tokens)
          (diagram-viewer-svg--reject "CSS token count exceeds the policy limit"))
        (let ((char (char-after)))
          (cond
           ((looking-at "[ \t\r\n]+")
            (goto-char (match-end 0)) (push '(space . " ") tokens))
           ((looking-at "/\\*")
            (forward-char 2)
            (unless (search-forward "*/" nil t)
              (diagram-viewer-svg--reject "Unterminated CSS comment"))
            (push '(space . " ") tokens))
           ((memq char '(?\" ?'))
            (let ((start (point)) (quote char))
              (forward-char)
              (while (and (not (eobp)) (/= (char-after) quote))
                (when (memq (char-after) '(?\\ ?\n ?\r ?\x00))
                  (diagram-viewer-svg--reject "CSS string escapes and controls are unsupported"))
                (forward-char))
              (when (eobp) (diagram-viewer-svg--reject "Unterminated CSS string"))
              (forward-char)
              (push (cons 'string (buffer-substring-no-properties start (point))) tokens)))
           ((looking-at "[A-Za-z_][A-Za-z0-9_-]*\\|--[A-Za-z_][A-Za-z0-9_-]*\\|-[A-Za-z_][A-Za-z0-9_-]*")
            (push (cons 'ident (match-string-no-properties 0)) tokens) (goto-char (match-end 0)))
           ((looking-at "#[A-Za-z0-9_-]+")
            (push (cons 'hash (match-string-no-properties 0)) tokens) (goto-char (match-end 0)))
           ((looking-at "\\(?:[0-9]+\\(?:\\.[0-9]*\\)?\\|\\.[0-9]+\\)\\(?:[eE][+-]?[0-9]+\\)?\\(?:[A-Za-z]+\\|%\\)?")
            (push (cons 'number (match-string-no-properties 0)) tokens) (goto-char (match-end 0)))
           ((memq char '(?{ ?} ?: ?\; ?\( ?\) ?, ?. ?> ?+ ?~ ?* ?/ ?! ?= ?\[ ?\] ?| ?^ ?$ ?- ?@))
            (push (cons 'punct (char-to-string char)) tokens) (forward-char))
           (t (diagram-viewer-svg--reject "Unsupported character or escape in CSS")))))
      (nreverse tokens))))

(defun diagram-viewer-svg--css-text (tokens)
  "Reconstruct CSS from the validated lexical TOKENS."
  (string-trim (mapconcat #'cdr tokens "")))

(defun diagram-viewer-svg--number (text)
  "Parse numeric TEXT only after bounding its lexical size."
  (when (> (length text) 64)
    (diagram-viewer-svg--reject "Numeric component is too long"))
  (string-to-number text))

(defun diagram-viewer-svg--css-value (tokens)
  "Validate a CSS declaration value represented by TOKENS."
  (let* ((items (vconcat (cl-remove-if (lambda (x) (eq (car x) 'space)) tokens)))
         (size (length items)) (i 0) stack)
    (when (zerop size) (diagram-viewer-svg--reject "Empty CSS value"))
    (while (< i size)
      (let* ((token (aref items i)) (text (cdr token)))
        (when (eq (car token) 'number)
          (when (> (abs (diagram-viewer-svg--number text))
                   (if (equal (car stack) "drop-shadow") 100 diagram-viewer-svg--max-number))
            (diagram-viewer-svg--reject "CSS numeric component exceeds the policy limit")))
        (when (member text '("{" "}" ";" "@" "[" "]"))
          (diagram-viewer-svg--reject "Unsupported token in CSS value"))
        (cond
         ((equal text "(")
          (unless (and (> i 0) (eq (car (aref items (1- i))) 'ident))
            (diagram-viewer-svg--reject "Unsupported CSS grouping"))
          (let ((name (downcase (cdr (aref items (1- i))))))
            (if (equal name "url")
                (progn
                  (unless (and (< (+ i 2) size) (equal (cdr (aref items (+ i 2))) ")"))
                    (diagram-viewer-svg--reject "Only local CSS fragment URLs are accepted"))
                  (let* ((target (aref items (1+ i)))
                         (value (if (eq (car target) 'string)
                                    (substring (cdr target) 1 -1) (cdr target))))
                    (unless (string-match-p "\\`#[A-Za-z0-9_-]+\\'" value)
                      (diagram-viewer-svg--reject "External CSS URL rejected")))
                  (setq i (+ i 2)))
              (unless (member name diagram-viewer-svg--css-functions)
                (diagram-viewer-svg--reject "Unsupported CSS function"))
              (push name stack))))
         ((equal text ")")
          (unless stack (diagram-viewer-svg--reject "Unbalanced CSS function"))
          (pop stack))))
      (cl-incf i))
    (when stack (diagram-viewer-svg--reject "Unclosed CSS function")))
  (diagram-viewer-svg--css-text tokens))

(defun diagram-viewer-svg--property-value (property tokens)
  "Validate TOKENS as the value of PROPERTY with property-specific limits."
  (let ((diagram-viewer-svg--max-number
         (if (equal property "box-shadow") 256 diagram-viewer-svg--max-number)))
    (diagram-viewer-svg--css-value tokens)))

(defun diagram-viewer-svg--geometry (value)
  "Reject numeric geometry components in VALUE outside the supported range."
  (let ((pos 0))
    (while (string-match "[+-]?\\(?:[0-9]+\\(?:\\.[0-9]*\\)?\\|\\.[0-9]+\\)\\(?:[eE][+-]?[0-9]+\\)?" value pos)
      (when (> (abs (diagram-viewer-svg--number (match-string 0 value))) diagram-viewer-svg--max-number)
        (diagram-viewer-svg--reject "SVG geometry exceeds the policy numeric limit"))
      (setq pos (match-end 0)))))

(defun diagram-viewer-svg--css (css &optional declarations-only)
  "Validate CSS and return a restricted reserialization.
DECLARATIONS-ONLY selects the grammar for a style attribute."
  (let* ((tokens (vconcat (diagram-viewer-svg--css-tokens css)))
         (pos 0) (size (length tokens)))
    (cl-labels
        ((skip () (while (and (< pos size) (eq (car (aref tokens pos)) 'space)) (cl-incf pos)))
         (peek () (skip) (and (< pos size) (cdr (aref tokens pos))))
         (expect (value)
           (unless (equal (peek) value) (diagram-viewer-svg--reject "Unexpected CSS structure"))
           (cl-incf pos))
         (take-until (delimiters)
           (let (found)
             (while (and (< pos size) (not (member (cdr (aref tokens pos)) delimiters)))
               (push (aref tokens pos) found) (cl-incf pos))
             (nreverse found)))
         (declarations (end)
           (let (output)
             (while (equal (peek) ";") (cl-incf pos))
             (while (and (peek) (not (equal (peek) end)))
               (let* ((token (aref tokens pos)) (property (downcase (cdr token))))
                 (unless (and (eq (car token) 'ident)
                              (or (member property diagram-viewer-svg--css-properties)
                                  (string-prefix-p "--mermaid-" property)))
                   (diagram-viewer-svg--reject
                    (concat "Unsupported CSS property: " (truncate-string-to-width property 60))))
                 (cl-incf pos) (expect ":")
                 (let* ((value (take-until '(";" "}")))
                        (checked (unless (string-empty-p (diagram-viewer-svg--css-text value))
                                   (diagram-viewer-svg--property-value property value))))
                   (when (and checked (not (string-prefix-p "animation" property)))
                     (push (concat property ":" checked ";") output)))
                 (while (equal (peek) ";") (cl-incf pos))))
             (when end (expect end))
             (apply #'concat (nreverse output))))
         (selector ()
           (let* ((items (take-until '("{"))) (text (diagram-viewer-svg--css-text items)))
             (when (or (string-empty-p text)
                       (cl-some (lambda (x) (member (cdr x) '("@" ";" "}"))) items))
               (diagram-viewer-svg--reject "Unsupported CSS selector"))
             (expect "{") text))
         (rule ()
           (if (equal (peek) "@")
               (progn
                 (cl-incf pos)
                 (unless (member (peek) '("keyframes" "-webkit-keyframes"))
                   (diagram-viewer-svg--reject "CSS imports and unknown at-rules are forbidden"))
                 (let ((keyword (peek)) name frames)
                   (cl-incf pos) (skip)
                   (unless (and (< pos size) (eq (car (aref tokens pos)) 'ident))
                     (diagram-viewer-svg--reject "Invalid CSS keyframe name"))
                   (setq name (peek)) (cl-incf pos) (expect "{")
                   (while (and (peek) (not (equal (peek) "}")))
                     (let ((label (selector)))
                       (unless (string-match-p "\\`\\(?:from\\|to\\|[0-9.]+%\\)\\(?:[ ]*,[ ]*\\(?:from\\|to\\|[0-9.]+%\\)\\)*\\'" label)
                         (diagram-viewer-svg--reject "Unsupported CSS keyframe selector"))
                       (push (concat label "{" (declarations "}") "}") frames)))
                   (expect "}")
                   ;; Validate the whole rule, but emit a static image.
                   (ignore keyword name frames)
                   ""))
             (let ((label (selector))) (concat label "{" (declarations "}") "}")))))
      (if declarations-only
          (let ((result (declarations nil)))
            (when (peek) (diagram-viewer-svg--reject "Unexpected CSS block")) result)
        (let (rules)
          (while (peek) (push (rule) rules))
          (apply #'concat (nreverse rules)))))))

(defun diagram-viewer-svg--qname (qname)
  "Return expanded QNAME, or reject a parser result without namespace data."
  (unless (and (consp qname) (stringp (car qname)) (stringp (cdr qname)))
    (diagram-viewer-svg--reject "XML namespace information is missing"))
  qname)

(defun diagram-viewer-svg--attribute (attribute)
  "Validate ATTRIBUTE and return its serialized spelling, or nil for xmlns."
  (let* ((qname (diagram-viewer-svg--qname (car attribute)))
         (namespace (car qname)) (name (cdr qname)) (value (cdr attribute)))
    (cond
     ((equal namespace diagram-viewer-svg--xmlns)
      (unless (or (equal value diagram-viewer-svg--namespace)
                  (equal value diagram-viewer-svg--xlink))
        (diagram-viewer-svg--reject "Unknown namespace declaration")) nil)
     ((equal namespace diagram-viewer-svg--xml)
      (unless (member name '("space" "lang"))
        (diagram-viewer-svg--reject "Unsupported XML attribute"))
      (concat " xml:" name "=\"" (diagram-viewer-svg--escape value) "\""))
     ((or (string-empty-p namespace) (equal namespace diagram-viewer-svg--xlink))
      (when (and (equal namespace diagram-viewer-svg--xlink) (not (equal name "href")))
        (diagram-viewer-svg--reject "Unsupported XLink attribute"))
      (cond
       ((equal name "href")
        (unless (string-match-p "\\`#[A-Za-z0-9_-]+\\'" value)
          (diagram-viewer-svg--reject "External SVG reference rejected")))
       ((equal name "style") (setq value (diagram-viewer-svg--css value t)))
       ((member name diagram-viewer-svg--css-properties)
        (unless (string-empty-p (string-trim value))
          (setq value (diagram-viewer-svg--property-value name (diagram-viewer-svg--css-tokens value)))))
       ((equal name "stdDeviation")
        (unless (and (string-match-p "\\`[0-9]+\\(?:\\.[0-9]+\\)?\\(?:[ ,]+[0-9]+\\(?:\\.[0-9]+\\)?\\)?\\'" value)
                     (cl-every (lambda (number) (<= (diagram-viewer-svg--number number) 100))
                               (split-string value "[ ,]+" t)))
          (diagram-viewer-svg--reject "Drop-shadow blur exceeds the supported range")))
       ((not (or (member name diagram-viewer-svg--attributes)
                 (string-match-p "\\`data-[a-z][a-z0-9_-]*\\'" name)))
        (diagram-viewer-svg--reject "Unsupported SVG attribute")))
      (when (member name '("viewBox" "x" "y" "x1" "y1" "x2" "y2" "dx" "dy"
                           "width" "height" "rx" "ry" "r" "cx" "cy" "d" "points"
                           "pathLength" "textLength" "markerWidth" "markerHeight" "refX" "refY"
                           "gradientTransform"))
        (diagram-viewer-svg--geometry value))
      (unless (or (string-prefix-p "animation" name)
                  (and (member name diagram-viewer-svg--css-properties)
                       (string-empty-p (string-trim value))))
        (concat " " name "=\"" (diagram-viewer-svg--escape value) "\"")))
     (t (diagram-viewer-svg--reject "Foreign attribute namespace rejected")))))

(defun diagram-viewer-svg--root-budget (attributes)
  "Validate root ATTRIBUTES against a conservative dimension and area budget."
  (let (width height viewbox)
    (dolist (attribute attributes)
      (let ((name (car attribute)) (value (cdr attribute)))
        (when (and (consp name) (equal (car name) ""))
          (cond
           ((member (cdr name) '("width" "height"))
            (unless (string-match "\\`\\([0-9]+\\(?:\\.[0-9]+\\)?\\)\\(px\\|%\\)?\\'" value)
              (diagram-viewer-svg--reject "Root dimensions must use bounded pixels or percentages"))
            (let ((number (diagram-viewer-svg--number (match-string 1 value)))
                  (unit (match-string 2 value)))
              (when (or (> number diagram-viewer-svg--max-number)
                        (and (equal unit "%") (> number 100)))
                (diagram-viewer-svg--reject "Root dimension exceeds the policy limit"))
              (unless (equal unit "%")
                (if (equal (cdr name) "width") (setq width number) (setq height number)))))
           ((equal (cdr name) "viewBox")
            (setq viewbox (split-string value "[ ,\\t\\r\\n]+" t))
            (unless (and (= (length viewbox) 4)
                         (cl-every (lambda (n) (string-match-p "\\`[+-]?[0-9]+\\(?:\\.[0-9]+\\)?\\'" n)) viewbox))
              (diagram-viewer-svg--reject "Unsupported SVG viewBox"))
            (setq viewbox (mapcar #'diagram-viewer-svg--number viewbox)))))))
    (when (or (> (* (or width 300) (or height 150)) (* 4096 4096))
              (and viewbox
                   (or (<= (nth 2 viewbox) 0) (<= (nth 3 viewbox) 0)
                       (> (* (nth 2 viewbox) (nth 3 viewbox)) (* 4096 4096)))))
      (diagram-viewer-svg--reject "SVG dimensions exceed the area budget"))))

(defun diagram-viewer-svg--serialize (node &optional root depth)
  "Validate NODE and serialize it, with ROOT and DEPTH constraints."
  (let* ((qname (diagram-viewer-svg--qname (car node)))
         (name (cdr qname)) (depth (or depth 1)))
    (unless (and (equal (car qname) diagram-viewer-svg--namespace)
                 (member name diagram-viewer-svg--elements)
                 (or (not root) (equal name "svg"))
                 (<= depth diagram-viewer-svg-max-depth))
      (diagram-viewer-svg--reject "Unsupported SVG element or namespace"))
    (when root (diagram-viewer-svg--root-budget (cadr node)))
    (let ((attributes
           (let (names output)
             (dolist (attribute (cadr node))
               (when-let* ((serialized (diagram-viewer-svg--attribute attribute)))
                 (unless (string-match "\\` \\([^=]+\\)=" serialized)
                   (diagram-viewer-svg--reject "Invalid serialized attribute"))
                 (let ((name (match-string 1 serialized)))
                   (when (member name names)
                     (diagram-viewer-svg--reject "Duplicate normalized SVG attribute"))
                   (push name names) (push serialized output))))
             (apply #'concat (nreverse output))))
          (children (cddr node)))
      (when (and (equal name "style")
                 (or (not (cl-every #'stringp children))
                     (let ((type (cl-find-if (lambda (a) (equal (cdr (car a)) "type"))
                                             (cadr node))))
                       (and type (not (equal (cdr type) "text/css"))))))
        (diagram-viewer-svg--reject "Only plain CSS style content is accepted"))
      (concat "<" name (if root " xmlns=\"http://www.w3.org/2000/svg\"" "") attributes ">"
              (if (equal name "style")
                  (diagram-viewer-svg--escape (diagram-viewer-svg--css (apply #'concat children)))
                (mapconcat
                 (lambda (child)
                   (if (stringp child) (diagram-viewer-svg--escape child)
                     (diagram-viewer-svg--serialize child nil (1+ depth)))) children ""))
              "</" name ">"))))

(defun diagram-viewer-svg-validate-file (path)
  "Read local regular file PATH and return a checked passive SVG in UTF-8.
Reject symlinks, oversized input, DTD, processing instructions, active SVG,
foreign namespaces and CSS outside the supported grammar.  Return fresh
unibyte XML, not the original bytes.  Signal `diagram-viewer-invalid-output'
for rejected content, or preserve file errors for Core storage diagnostics."
  (unless (and (stringp path) (not (file-remote-p path))
               (not (file-symlink-p path)) (file-regular-p path))
    (diagram-viewer-svg--reject "SVG must be a local regular non-symlink file"))
  (let ((bytes (with-temp-buffer
                 (set-buffer-multibyte nil)
                 (insert-file-contents-literally path nil 0 (1+ diagram-viewer-svg-max-bytes))
                 (buffer-string))))
    (unless (and (> (length bytes) 0) (<= (length bytes) diagram-viewer-svg-max-bytes))
      (diagram-viewer-svg--reject "SVG is empty or exceeds the policy size limit"))
    (condition-case err
        (let ((text (decode-coding-string bytes 'utf-8-unix)))
          (unless (cl-loop for char across text
                           always (diagram-viewer-svg--xml-char-p char))
            (diagram-viewer-svg--reject "Only valid UTF-8 XML text is accepted"))
          (diagram-viewer-svg--preflight text)
          (diagram-viewer-svg--entities text)
          (with-temp-buffer
            (insert (string-remove-prefix "\ufeff" text))
            (let* ((xml-default-ns '(("" . "")
                                     ("xml" . "http://www.w3.org/XML/1998/namespace")
                                     ("xmlns" . "http://www.w3.org/2000/xmlns/")))
                   (tree (xml-parse-region (point-min) (point-max) nil nil t)))
              (unless (= (length tree) 1)
                (diagram-viewer-svg--reject "SVG must have exactly one document element"))
              (let ((output (encode-coding-string
                             (diagram-viewer-svg--serialize (car tree) t) 'utf-8-unix)))
                (when (> (length output) diagram-viewer-svg-max-bytes)
                  (diagram-viewer-svg--reject "Normalized SVG exceeds the policy size limit"))
                output))))
      (diagram-viewer-invalid-output (signal (car err) (cdr err)))
      (error (diagram-viewer-svg--reject "Malformed or unsupported XML/SVG/CSS")))))

(provide 'diagram-viewer-svg)
;;; diagram-viewer-svg.el ends here
