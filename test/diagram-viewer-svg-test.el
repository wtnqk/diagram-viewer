;;; diagram-viewer-svg-test.el --- Restricted SVG security tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Adversarial fixtures are never sent to an image decoder.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'diagram-viewer)
(require 'diagram-viewer-svg)

(defconst diagram-viewer-svg-test--fixture-directory
  (expand-file-name "fixtures" (file-name-directory (or load-file-name buffer-file-name))))

(defconst diagram-viewer-svg-test--root
  "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"120\" height=\"80\" viewBox=\"0 0 120 80\">")

(defun diagram-viewer-svg-test--validate (content &optional raw)
  "Validate CONTENT in a temporary file, writing literal bytes if RAW."
  (let ((path (make-temp-file "diagram-viewer-svg-test-" nil ".svg")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write (if raw 'no-conversion 'utf-8-unix)))
            (write-region content nil path nil 'silent))
          (diagram-viewer-svg-validate-file path))
      (unless (string-prefix-p "diagram-viewer-svg-test-" (file-name-nondirectory path))
        (error "Unexpected fixture path"))
      (delete-file path))))

(defun diagram-viewer-svg-test--wrap (body)
  "Return an SVG document containing BODY."
  (concat diagram-viewer-svg-test--root body "</svg>"))

(ert-deftest diagram-viewer-svg-passive-xml-is-normalized ()
  (let ((result (diagram-viewer-svg-test--validate
                 (diagram-viewer-svg-test--wrap
                  "<!--comment--><rect x=\"1\" y=\"2\" width=\"10\" height=\"20\" fill=\"#f00\"/><text x=\"2\" y=\"3\">日本語 &amp; &lt;script&gt;</text>"))))
    (should-not (multibyte-string-p result))
    (should (string-match-p "&lt;script&gt;" result))
    (should-not (string-match-p "<!--" result))
    (should (equal result (diagram-viewer-svg-test--validate result t)))))

(ert-deftest diagram-viewer-svg-ascii-output-is-unibyte ()
  (let ((bytes (diagram-viewer-svg-test--validate
                (diagram-viewer-svg-test--wrap "<rect width='10' height='10'/>"))))
    (should (stringp bytes))
    (should-not (multibyte-string-p bytes))))

(ert-deftest diagram-viewer-svg-xml-declaration-and-cdata ()
  (should (diagram-viewer-svg-test--validate
           (concat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                   (diagram-viewer-svg-test--wrap "<text><![CDATA[<script>inert</script>]]></text>"))))
  (should-error
   (diagram-viewer-svg-test--validate
    (concat "<?xml version=\"1.0\" encoding=\"UTF-16\"?>" (diagram-viewer-svg-test--wrap "")))
   :type 'diagram-viewer-invalid-output))

(ert-deftest diagram-viewer-svg-rejects-active-elements ()
  (dolist (element '("script" "foreignObject" "iframe" "object" "embed" "image" "use"
                     "animate" "animateMotion" "animateTransform" "set" "feImage"))
    (ert-info ((format "Element: %s" element))
              (should-error (diagram-viewer-svg-test--validate
                             (diagram-viewer-svg-test--wrap (format "<%s/>" element)))
                            :type 'diagram-viewer-invalid-output))))

(ert-deftest diagram-viewer-svg-rejects-events-and-foreign-attributes ()
  (dolist (attribute '("onload=\"alert(1)\"" "ONCLICK=\"run()\"" "xml:base=\"https://example.invalid/\""
                       "xmlns:a=\"urn:attacker\" a:fill=\"red\"" "unknown=\"1\""))
    (should-error (diagram-viewer-svg-test--validate
                   (diagram-viewer-svg-test--wrap (concat "<g " attribute "/>")))
                  :type 'diagram-viewer-invalid-output)))

(ert-deftest diagram-viewer-svg-rejects-dtd-entities-and-processing-instructions ()
  (dolist (prefix '("<!DOCTYPE svg SYSTEM 'https://example.invalid/a.dtd'>"
                    "<!DOCTYPE svg [<!ENTITY x SYSTEM 'file:///etc/passwd'>]>"
                    "<?xml-stylesheet href='https://example.invalid/a.css'?>"))
    (should-error (diagram-viewer-svg-test--validate
                   (concat prefix (diagram-viewer-svg-test--wrap "")))
                  :type 'diagram-viewer-invalid-output)))

(ert-deftest diagram-viewer-svg-rejects-malformed-xml ()
  (dolist (document (list "" "<svg/>" "<svg xmlns='urn:wrong'/>"
                          (concat (diagram-viewer-svg-test--wrap "") "<g/>")
                          (diagram-viewer-svg-test--wrap "<g></rect>")
                          (diagram-viewer-svg-test--wrap "<g id='a' id='b'/>")
                          (diagram-viewer-svg-test--wrap "<text>&undeclared;</text>")))
    (should-error (diagram-viewer-svg-test--validate document)
                  :type 'diagram-viewer-invalid-output)))

(ert-deftest diagram-viewer-svg-rejects-external-reference-forms ()
  (dolist (url '("https://example.invalid/a.svg" "http://example.invalid" "file:///etc/passwd"
                 "javascript:alert(1)" "data:image/svg+xml,test" "../secret" "//example.invalid/a"))
    (dolist (attribute '("href" "xlink:href"))
      (should-error
       (diagram-viewer-svg-test--validate
        (diagram-viewer-svg-test--wrap
         (format "<linearGradient xmlns:xlink='http://www.w3.org/1999/xlink' %s='%s'/>" attribute url)))
       :type 'diagram-viewer-invalid-output))))

(ert-deftest diagram-viewer-svg-namespace-aliases-and-rebinding ()
  (should (diagram-viewer-svg-test--validate
           "<s:svg xmlns:s='http://www.w3.org/2000/svg'><s:g/></s:svg>"))
  (should-error (diagram-viewer-svg-test--validate
                 (diagram-viewer-svg-test--wrap
                  "<g xmlns='urn:foreign'><script/></g>"))
                :type 'diagram-viewer-invalid-output)
  (should-error (diagram-viewer-svg-test--validate
                 (diagram-viewer-svg-test--wrap
                  "<linearGradient xmlns:a='http://www.w3.org/1999/xlink' a:href='https://example.invalid'/>"))
                :type 'diagram-viewer-invalid-output)
  (should-error (diagram-viewer-svg-test--validate
                 (diagram-viewer-svg-test--wrap
                  "<linearGradient xmlns:a='http://www.w3.org/1999/xlink' href='#a' a:href='#b'/>"))
                :type 'diagram-viewer-invalid-output))

(ert-deftest diagram-viewer-svg-safe-local-resource-reference ()
  (should (diagram-viewer-svg-test--validate
           (diagram-viewer-svg-test--wrap
            "<defs><marker id='arrow'><path d='M0 0L10 5L0 10Z'/></marker></defs><path d='M0 0L20 20' marker-end='url(#arrow)'/>")))
  (should (diagram-viewer-svg-test--validate
           (diagram-viewer-svg-test--wrap
            "<defs><filter id='s'><feDropShadow stdDeviation='2' dx='1' dy='1'/></filter></defs><g filter='url(&quot;#s&quot;)'/>"))))

(ert-deftest diagram-viewer-svg-css-safe-grammar ()
  (let ((result (diagram-viewer-svg-test--validate
                 (diagram-viewer-svg-test--wrap
                  "<style>#root .label{font-family:&quot;trebuchet ms&quot;,sans-serif;font-size:16px;fill:rgb(1,2,3);;filter:drop-shadow(0px 2px 2px rgba(0,0,0,0.2));}</style><g id='root' style='fill:#fff;stroke-width:1;'/>"))))
    (should result)))

(ert-deftest diagram-viewer-svg-css-border-side-properties ()
  (dolist (side '("top" "right" "bottom" "left"))
    (dolist (entry '(("" . "1px solid rgb(10,20,30)")
                     ("-width" . "2px") ("-style" . "solid")
                     ("-color" . "rgba(10,20,30,0.5)")))
      (let* ((property (concat "border-" side (car entry)))
             (declaration (concat property ":" (cdr entry) ";"))
             (bytes (diagram-viewer-svg-test--validate
                     (diagram-viewer-svg-test--wrap
                      (format "<style>.stateGroup{%s}</style><g style='%s'/>"
                              declaration declaration)))))
        (should-not (multibyte-string-p bytes))
        (should (string-match-p (regexp-quote declaration) bytes))))))

(ert-deftest diagram-viewer-svg-css-state-border-bottom-without-semicolon ()
  ;; Mermaid stateDiagram-v2 emits this rule, even for a simple state diagram.
  (let ((bytes (diagram-viewer-svg-test--validate
                (diagram-viewer-svg-test--wrap
                 "<style>.stateGroup .composit{fill:#fff; border-bottom: 1px}</style><g style='BORDER-BOTTOM: 1px'/>"))))
    (should (string-match-p "border-bottom:1px;" bytes))))

(ert-deftest diagram-viewer-svg-css-border-sides-still-reject-unsafe-values ()
  (dolist (side '("top" "right" "bottom" "left"))
    (dolist (value '("url(https://example.invalid/a)" "URL(file:///etc/passwd)"
                     "u\\72l(https://example.invalid/a)" "expression(alert(1))"
                     "calc(1px * 1000000000)" "1e100px solid red"))
      (dolist (body (list (format "<style>g{border-%s:%s}</style>" side value)
                          (format "<g style='border-%s:%s'/>" side value)
                          (format "<g border-%s='%s'/>" side value)))
        (should-error (diagram-viewer-svg-test--validate (diagram-viewer-svg-test--wrap body))
                      :type 'diagram-viewer-invalid-output)))))

(ert-deftest diagram-viewer-svg-css-state-corner-radii ()
  (let ((bytes (diagram-viewer-svg-test--validate
                (diagram-viewer-svg-test--wrap
                 "<style>.stateGroup rect{rx:5px;ry:5px}</style><rect width='10' height='10' style='rx:2;ry:3'/>"))))
    (should (string-match-p "rx:5px;ry:5px;" bytes))
    (should (string-match-p "rx:2;ry:3;" bytes))))

(ert-deftest diagram-viewer-svg-css-corner-radii-still-reject-unsafe-values ()
  (dolist (property '("rx" "ry"))
    (dolist (value '("url(https://example.invalid/a)" "expression(alert(1))"
                     "calc(10000px * 10000)" "1e100px"))
      (dolist (body (list (format "<style>rect{%s:%s}</style>" property value)
                          (format "<rect style='%s:%s'/>" property value)
                          (format "<rect %s='%s'/>" property value)))
        (should-error (diagram-viewer-svg-test--validate (diagram-viewer-svg-test--wrap body))
                      :type 'diagram-viewer-invalid-output)))))

(ert-deftest diagram-viewer-svg-css-border-family-is-not-a-wildcard ()
  (dolist (property '("border-image" "border-image-source" "border-image-width"
                      "border-unknown"))
    (should-error (diagram-viewer-svg--css (format "g{%s:1px;}" property))
                  :type 'diagram-viewer-invalid-output)))

(ert-deftest diagram-viewer-svg-css-rejects-obfuscated-urls-and-imports ()
  (dolist (css '("g{fill:url(https://example.invalid/a);}"
                 "g{fill:URL(https://example.invalid/a);}"
                 "g{fill:u/**/rl(https://example.invalid/a);}"
                 "g{fill:u\\72l(https://example.invalid/a);}"
                 "g{fill:url('data:image/png,a');}"
                 "g{fill:image-set('https://example.invalid/a' 1x);}"
                 "g{fill:expression(alert(1));}"
                 "@import 'https://example.invalid/a.css';"
                 "@font-face{font-family:x;src:url(https://example.invalid/a);}"
                 "g{-moz-binding:url(https://example.invalid/a);}"
                 "g{fill:var(--remote);}"))
    (ert-info ((format "CSS input rejected: %s" css))
              (should-error (diagram-viewer-svg-test--validate
                             (diagram-viewer-svg-test--wrap (concat "<style>" css "</style>")))
                            :type 'diagram-viewer-invalid-output))))

(ert-deftest diagram-viewer-svg-css-entities-cannot-hide-url ()
  (should-error
   (diagram-viewer-svg-test--validate
    (diagram-viewer-svg-test--wrap "<g style='fill:u&#114;l(https://example.invalid/a)'/>"))
   :type 'diagram-viewer-invalid-output))

(ert-deftest diagram-viewer-svg-css-animation-is-staticized ()
  (let ((result (diagram-viewer-svg-test--validate
                 (diagram-viewer-svg-test--wrap
                  "<style>@keyframes zoom{from{stroke-width:1;}to{stroke-width:100;}}rect{animation:zoom 1ms infinite;fill:red;}</style><rect width='10' height='10'/>"))))
    (should-not (string-match-p "@keyframes\\|animation:" result))
    (should (string-match-p "fill:red" result))))

(ert-deftest diagram-viewer-svg-resource-limits ()
  (dolist (body '("<rect width='1e100' height='10'/>"
                  "<path d='M0 0 L1e100 5'/>"
                  "<filter><feDropShadow stdDeviation='101'/></filter>"
                  "<g style='filter:drop-shadow(0px 0px 1e100px red)'/>"
                  "<g style='filter:drop-shadow(0px 0px calc(100px * 1000000000) red)'/>"
                  "<g style='box-shadow:0px 0px 10000px red'/>"))
    (should-error (diagram-viewer-svg-test--validate (diagram-viewer-svg-test--wrap body))
                  :type 'diagram-viewer-invalid-output))
  (should-error (diagram-viewer-svg-test--validate
                 "<svg xmlns='http://www.w3.org/2000/svg' width='16000' height='16000'/>")
                :type 'diagram-viewer-invalid-output)
  (let ((diagram-viewer-svg-max-bytes 16))
    (should-error (diagram-viewer-svg-test--validate (diagram-viewer-svg-test--wrap ""))
                  :type 'diagram-viewer-invalid-output))
  (let ((diagram-viewer-svg-max-bytes 250))
    (should-error
     (diagram-viewer-svg-test--validate
      (diagram-viewer-svg-test--wrap (concat "<text><![CDATA[" (make-string 70 ?<) "]]></text>")))
     :type 'diagram-viewer-invalid-output))
  (let ((diagram-viewer-svg-max-depth 3))
    (should-error (diagram-viewer-svg-test--validate (diagram-viewer-svg-test--wrap "<g><g><g/></g></g>"))
                  :type 'diagram-viewer-invalid-output))
  (let ((diagram-viewer-svg-max-nodes 2))
    (should-error (diagram-viewer-svg-test--validate (diagram-viewer-svg-test--wrap "<g/><g/>"))
                  :type 'diagram-viewer-invalid-output))
  (let ((diagram-viewer-svg-max-css-tokens 5))
    (should-error (diagram-viewer-svg--css "g{fill:red;stroke:blue;}")
                  :type 'diagram-viewer-invalid-output)))

(ert-deftest diagram-viewer-svg-rejects-invalid-encodings-and-character-entities ()
  (dolist (bytes (list (unibyte-string #xff #xfe 0 1) (unibyte-string #xc0 #xaf)))
    (should-error (diagram-viewer-svg-test--validate bytes t) :type 'diagram-viewer-invalid-output))
  (should-error (diagram-viewer-svg-test--validate
                 (diagram-viewer-svg-test--wrap "<text>&#0;</text>"))
                :type 'diagram-viewer-invalid-output))

(ert-deftest diagram-viewer-svg-rejects-symlink-and-nonfiles ()
  (should-error (diagram-viewer-svg-validate-file temporary-file-directory)
                :type 'diagram-viewer-invalid-output)
  (let* ((path (make-temp-file "diagram-viewer-svg-test-" nil ".svg"))
         (link (concat path ".link")))
    (unwind-protect
        (progn (make-symbolic-link path link)
               (should-error (diagram-viewer-svg-validate-file link)
                             :type 'diagram-viewer-invalid-output))
      (delete-file link) (delete-file path))))

(ert-deftest diagram-viewer-svg-numeric-css-lexer-is-anchored ()
  ;; Not a timing assertion: instrument the obsolete non-anchored operation.
  (cl-letf (((symbol-function 'string-match)
             (lambda (&rest _) (ert-fail "Lexer must use anchored buffer scans"))))
    (should (= 3999 (length (diagram-viewer-svg--css-tokens
                             (mapconcat #'identity (make-list 2000 "1") " ")))))))

(ert-deftest diagram-viewer-svg-real-mermaid-fixtures ()
  (let ((directory diagram-viewer-svg-test--fixture-directory))
    (dolist (name '("mermaid-flowchart.svg" "mermaid-sequence.svg" "mermaid-class.svg"
                    "mermaid-state.svg"))
      (let* ((path (expand-file-name name directory))
             (bytes (diagram-viewer-svg-validate-file path)))
        (should (> (length bytes) 100))
        (should-not (string-match-p "foreignObject\\|<script\\|@keyframes\\|animation:" bytes))
        (should (equal bytes (diagram-viewer-svg-test--validate bytes t)))))))

(provide 'diagram-viewer-svg-test)
;;; diagram-viewer-svg-test.el ends here
