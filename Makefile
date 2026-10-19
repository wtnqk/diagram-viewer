EMACS ?= emacs
LISP_SOURCES = diagram-viewer.el diagram-viewer-svg.el diagram-viewer-viewer.el diagram-viewer-adapter-mermaid.el diagram-viewer-adapter-md-mode.el
TESTS = test/diagram-viewer-test.el test/diagram-viewer-jobs-test.el test/diagram-viewer-commands-test.el test/diagram-viewer-svg-test.el test/diagram-viewer-mermaid-test.el test/diagram-viewer-integration-test.el test/diagram-viewer-layout-test.el test/diagram-viewer-md-mode-test.el test/diagram-viewer-viewer-test.el test/diagram-viewer-ui-integration-test.el
MD_MODE_DIR ?=
TEXTUI_DIR ?=
MD_LOAD_PATH = $(if $(MD_MODE_DIR),-L "$(MD_MODE_DIR)") $(if $(TEXTUI_DIR),-L "$(TEXTUI_DIR)")
TEST_LOAD = $(foreach file,$(TESTS),-l $(file))
COMPILED_LOAD = $(foreach file,$(LISP_SOURCES),-l build/$(notdir $(file:.el=.elc)))

.PHONY: test test-compiled test-real test-layout-gui test-viewer-gui test-ui-gui compile checkdoc check-docs check

test:
	$(EMACS) -Q --batch -L . -L test $(MD_LOAD_PATH) $(TEST_LOAD) -f ert-run-tests-batch-and-exit

compile:
	mkdir -p build
	$(EMACS) -Q --batch -L . --eval '(progn (require (quote bytecomp)) (let ((byte-compile-error-on-warn t) (byte-compile-dest-file-function (lambda (source) (expand-file-name (concat (file-name-base source) ".elc") "build")))) (dolist (file (quote ("diagram-viewer.el" "diagram-viewer-svg.el" "diagram-viewer-viewer.el" "diagram-viewer-adapter-mermaid.el" "diagram-viewer-adapter-md-mode.el"))) (unless (byte-compile-file file) (kill-emacs 1)))))'

test-compiled: compile
	$(EMACS) -Q --batch -L . -L test $(MD_LOAD_PATH) $(COMPILED_LOAD) $(TEST_LOAD) -f ert-run-tests-batch-and-exit

checkdoc:
	$(EMACS) -Q --batch --eval '(progn (require (quote checkdoc)) (let ((warning-minimum-level :emergency) issues) (cl-letf (((symbol-function (quote display-warning)) (lambda (&rest args) (push args issues)))) (dolist (file (quote ("diagram-viewer.el" "diagram-viewer-svg.el" "diagram-viewer-viewer.el" "diagram-viewer-adapter-mermaid.el" "diagram-viewer-adapter-md-mode.el"))) (checkdoc-file file))) (dolist (issue (reverse issues)) (princ (format "%S\n" issue))) (kill-emacs (if issues 1 0))))'

check-docs:
	$(EMACS) -Q --batch -l test/check-docs.el

test-real:
	DIAGRAM_VIEWER_REAL_MERMAID=1 $(EMACS) -Q --batch -L . -L test $(MD_LOAD_PATH) -l test/diagram-viewer-mermaid-test.el -l test/diagram-viewer-integration-test.el -f ert-run-tests-batch-and-exit

test-layout-gui:
	timeout -k 2s 30s env GDK_BACKEND=wayland GSETTINGS_BACKEND=memory $(EMACS) -Q -l test/diagram-viewer-layout-test.el -f dv-layout-prototype-gui-run

test-viewer-gui:
	timeout -k 2s 45s env GDK_BACKEND=wayland GSETTINGS_BACKEND=memory $(EMACS) -Q -L . -L test -l test/diagram-viewer-viewer-test.el --eval '(diagram-viewer-viewer-test-run-gui)'

test-ui-gui:
	timeout -k 2s 100s env GDK_BACKEND=wayland GSETTINGS_BACKEND=memory $(EMACS) -Q -L . -L test $(MD_LOAD_PATH) -l test/diagram-viewer-ui-integration-test.el -f diagram-viewer-ui-integration-gui-run

check: test test-compiled checkdoc check-docs
