;;; diagram-viewer-layout-test.el --- Isolated layout experiment -*- lexical-binding: t; -*-

;;; Commentary:
;; Prototype only: no Core/Adapter dependency and no hooks on user buffers.
;; Batch: emacs -Q --batch -l test/diagram-viewer-layout-test.el \
;;          -f ert-run-tests-batch-and-exit
;; GUI: see docs/LAYOUT-PROTOTYPE.org for a bounded, isolated invocation.
;; The candidates are experiments, not an accepted right-corner layout.

;;; Code:
(require 'cl-lib)
(require 'ert)
(require 'subr-x)

(defconst dv-layout--source "```mermaid\ngraph TD; A-->B\n```\n")
(defconst dv-layout--placeholder "\n \n\n")
(defvar dv-layout--last-activation nil)

(defun dv-layout--svg (width height)
  "Return an inert, fixed rectangle SVG with WIDTH and HEIGHT."
  (unless (and (integerp width) (<= 1 width 800)
               (integerp height) (<= 1 height 400))
    (error "Invalid fixture dimensions"))
  (format (concat "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" "
                  "height=\"%d\"><rect width=\"100%%\" height=\"100%%\" "
                  "fill=\"#d8e8f8\" stroke=\"#173958\"/></svg>")
          width height))

(defun dv-layout--image (width height)
  "Create a WIDTH by HEIGHT fixture image, without file or renderer IO."
  (create-image (dv-layout--svg width height) 'svg t :ascent 'center))

(defun dv-layout--fixture (&optional image)
  "Insert one revision-specific placeholder with optional IMAGE.
This is fixture construction, not the decoration under test."
  (let ((start (point)))
    (insert dv-layout--placeholder)
    (add-text-properties start (+ start 4)
                         `(md-render-source ,dv-layout--source
                           md-render-frozen t))
    (put-text-property (1+ start) (+ start 2) 'md-render-media-file
                       "/fixture/never-read.png")
    (when image
      (put-text-property (1+ start) (+ start 2) 'display image))
    (1+ start)))

(defun dv-layout--valid-p (overlay)
  "Check that OVERLAY still identifies its original placeholder."
  (let* ((marker (overlay-get overlay 'dv-layout-target))
         (buffer (and (markerp marker) (marker-buffer marker)))
         (pos (and buffer (marker-position marker))))
    (and (eq (overlay-get overlay 'dv-layout-owner) 'prototype)
         (eq buffer (overlay-buffer overlay))
         pos
         (with-current-buffer buffer
           (save-restriction
             (widen)
             (and (> pos (point-min)) (<= (+ pos 3) (point-max))
                  (= pos (overlay-start overlay))
                  (= (1+ pos) (overlay-end overlay))
                  (equal (buffer-substring-no-properties (1- pos) (+ pos 3))
                         dv-layout--placeholder)
                  (equal (get-text-property pos 'md-render-source)
                         (overlay-get overlay 'dv-layout-source))
                  (cl-loop for p from (1- pos) below (+ pos 3)
                           always (and (get-text-property p 'md-render-frozen)
                                       (equal (get-text-property p 'md-render-source)
                                              (overlay-get overlay 'dv-layout-source))))
                  (equal (get-text-property pos 'md-render-media-file)
                         (overlay-get overlay 'dv-layout-media))))))))

(defun dv-layout--activate (overlay window)
  "Return current target data for OVERLAY in WINDOW, or reject it.
No viewer, renderer, selection or buffer edit is performed."
  (unless (and (window-live-p window)
               (eq (window-buffer window) (overlay-buffer overlay))
               (or (null (overlay-get overlay 'window))
                   (eq window (overlay-get overlay 'window)))
               (dv-layout--valid-p overlay))
    (user-error "Prototype target is stale or belongs to another window"))
  (let ((marker (overlay-get overlay 'dv-layout-target)))
    (setq dv-layout--last-activation
          (list :buffer (marker-buffer marker) :position (marker-position marker)
                :window window :source (get-text-property
                                        marker 'md-render-source
                                        (marker-buffer marker))))))

(defun dv-layout--ret ()
  "Activate the window-local prototype at point, not a nearby image."
  (interactive)
  (let ((overlay (cl-find-if
                  (lambda (ov)
                    (and (eq (overlay-get ov 'dv-layout-owner) 'prototype)
                         (eq (overlay-get ov 'window) (selected-window))))
                  (overlays-at (point)))))
    (unless overlay (user-error "No prototype button at point"))
    (dv-layout--activate overlay (selected-window))))

(defun dv-layout--mouse (event)
  "Resolve EVENT through its displayed string, not the current point."
  (interactive "e")
  (let* ((position (event-start event))
         (string-pos (posn-string position))
         (overlay (and string-pos
                       (get-text-property (cdr string-pos) 'dv-layout-overlay
                                          (car string-pos)))))
    (unless (overlayp overlay) (user-error "Not a prototype button"))
    (dv-layout--activate overlay (posn-window position))))

(defvar dv-layout--map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'dv-layout--mouse)
    (define-key map (kbd "RET") #'dv-layout--ret)
    map))

(defun dv-layout--clear ()
  "Remove only prototype overlays and release their markers."
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (when (eq (overlay-get overlay 'dv-layout-owner) 'prototype)
      (let ((marker (overlay-get overlay 'dv-layout-target)))
        (when (markerp marker) (set-marker marker nil)))
      (delete-overlay overlay))))

(defun dv-layout--attach (pos window candidate)
  "Try CANDIDATE around image at POS in WINDOW, without changing text.
CANDIDATE is plain-after, raised-after, aligned-after or aligned-before.
No candidate is claimed to overlap the image.  Missing images produce a
non-displaying target overlay; there is no adjacent-button fallback."
  (unless (memq candidate '(plain-after raised-after aligned-after aligned-before))
    (error "Unknown candidate: %S" candidate))
  (let* ((overlay (make-overlay pos (1+ pos) nil nil nil))
         (image (get-text-property pos 'display))
         (size (and (consp image) (eq (car image) 'image)
                    (image-size image t (window-frame window))))
         (button (propertize "[V]" 'face 'link 'mouse-face 'highlight
                             'keymap dv-layout--map 'help-echo "Prototype target")))
    (overlay-put overlay 'dv-layout-owner 'prototype)
    (overlay-put overlay 'dv-layout-target (copy-marker pos))
    (overlay-put overlay 'dv-layout-source (get-text-property pos 'md-render-source))
    (overlay-put overlay 'dv-layout-media (get-text-property pos 'md-render-media-file))
    (overlay-put overlay 'window window)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'keymap dv-layout--map)
    (overlay-put overlay 'dv-layout-candidate candidate)
    (put-text-property 0 (length button) 'dv-layout-overlay overlay button)
    (when size
      (let* ((font-height (frame-char-height (window-frame window)))
             (rise (/ (- (/ (cdr size) 2.0) (/ font-height 2.0)) font-height))
             (x (max 0 (- (car size) (string-pixel-width button))))
             (padding (propertize " " 'display `(space :align-to (,x))))
             (string
              (if (eq candidate 'plain-after)
                  button
                (put-text-property 0 (length button) 'display `(raise ,rise) button)
                (if (memq candidate '(aligned-after aligned-before))
                    (concat padding button)
                  button))))
        (overlay-put overlay 'dv-layout-size size)
        (overlay-put overlay (if (eq candidate 'aligned-before)
                                 'before-string 'after-string)
                     string)))
    overlay))

(defun dv-layout--snapshot ()
  "Return the buffer state that decoration must preserve."
  (list (buffer-substring-no-properties (point-min) (point-max))
        (cl-loop for pos from (point-min) below (point-max)
                 collect (copy-tree (text-properties-at pos)))
        (buffer-modified-p) (copy-tree buffer-undo-list) (point)
        (point-min) (point-max) (buffer-chars-modified-tick)))

(defun dv-layout--object-box (position)
  "Return glyph geometry from actual hit POSITION, distinct from row hit area."
  (let ((xy (posn-x-y position))
        (offset (posn-object-x-y position))
        (size (posn-object-width-height position)))
    (when (and xy offset size)
      (list (- (car xy) (car offset)) (- (cdr xy) (cdr offset))
            (car size) (cdr size)))))

(defun dv-layout--image-box (pos window)
  "Read the actual image glyph at POS in WINDOW, ignoring before strings."
  (let ((image (get-text-property pos 'display (window-buffer window))))
    (catch 'found
      (cl-loop for y from 0 below (min 300 (window-body-height window t)) by 5 do
               (cl-loop for x from 0 below (min 640 (window-body-width window t)) by 5 do
                        (let ((position (posn-at-x-y x y window)))
                          (when (and position (equal (posn-image position) image)
                                     (equal (posn-point position) pos))
                            (throw 'found (dv-layout--object-box position))))))
      nil)))

(defun dv-layout--button-box (overlay window)
  "Measure OVERLAY's actual hit region by pixel probing in WINDOW.
Scan at most 640 by 300 pixels.  Return (X Y WIDTH HEIGHT SAMPLE).
SAMPLE is a genuine position object suitable for a synthetic mouse event."
  (let ((max-x (min 640 (window-body-width window t)))
        (max-y (min 300 (window-body-height window t)))
        min-x min-y end-x end-y sample)
    (cl-loop for y from 0 below max-y by 2 do
             (cl-loop for x from 0 below max-x by 2 do
                      (let* ((p (posn-at-x-y x y window))
                             (s (and p (posn-string p))))
                        (when (and s (eq (get-text-property
                                          (cdr s) 'dv-layout-overlay (car s))
                                         overlay))
                          (setq min-x (if min-x (min min-x x) x)
                                min-y (if min-y (min min-y y) y)
                                end-x (if end-x (max end-x x) x)
                                end-y (if end-y (max end-y y) y)
                                sample p)))))
    (when sample
      (let ((left min-x) (top min-y) (right end-x) (bottom end-y))
        (cl-loop for y from (max 0 (1- top)) to (min (1- max-y) (1+ bottom)) do
                 (cl-loop for x from (max 0 (1- left)) to (min (1- max-x) (1+ right)) do
                          (let* ((p (posn-at-x-y x y window))
                                 (s (and p (posn-string p))))
                            (when (and s (eq (get-text-property
                                              (cdr s) 'dv-layout-overlay (car s)) overlay))
                              (setq min-x (min min-x x) min-y (min min-y y)
                                    end-x (max end-x x) end-y (max end-y y))))))
        (list min-x min-y (1+ (- end-x min-x)) (1+ (- end-y min-y)) sample)))))

(defun dv-layout--report (value)
  "Print VALUE to stderr, including in a graphical Emacs process."
  (princ (concat (prin1-to-string value) "\n") #'external-debugging-output))

(defun dv-layout-prototype-gui-run ()
  "Measure candidates in this disposable GUI process, then exit.
Use only in an isolated `emacs -Q' invocation.  Never load into a user's
server: this runner deliberately replaces its own initial window layout."
  (interactive)
  (let ((buffer (generate-new-buffer " *dv-layout-fixture*"))
        (failures 0))
    (unwind-protect
        (condition-case err
            (progn
              (unless (and (display-graphic-p) (image-type-available-p 'svg))
                (error "GUI/SVG unavailable"))
              (dv-layout--report (list :environment emacs-version window-system))
              (set-frame-size (selected-frame) 110 35)
              (delete-other-windows)
              (switch-to-buffer buffer)
              (setq-local mode-line-format nil header-line-format nil
                          truncate-lines t cursor-type nil)
              (let* ((pos (dv-layout--fixture (dv-layout--image 200 100)))
                     (second-pos (dv-layout--fixture (dv-layout--image 120 60)))
                     (left (selected-window))
                     (right (split-window-right 65)))
                (set-window-buffer right buffer)
                (set-window-point left pos)
                (set-window-point right pos)
                (dolist (dimensions '((200 100) (280 160)))
                  (with-silent-modifications
                    (put-text-property pos (1+ pos) 'display
                                       (apply #'dv-layout--image dimensions)))
                  (dolist (candidate '(plain-after raised-after aligned-after aligned-before))
                    (dv-layout--clear)
                    (let ((before (dv-layout--snapshot)))
                      (dolist (window (list left right))
                        (with-selected-window window
                          (dv-layout--attach pos window candidate)))
                      (unless (equal before (dv-layout--snapshot))
                        (error "Decoration changed buffer state")))
                    (sit-for 0.05)
                    (redisplay t)
                    (dolist (window (list left right))
                      (let* ((overlay (cl-find-if
                                       (lambda (ov) (eq (overlay-get ov 'window) window))
                                       (overlays-at pos)))
                             (image-box (dv-layout--image-box pos window))
                             (button-box (dv-layout--button-box overlay window)))
                        (unless (and image-box button-box)
                          (dv-layout--report
                           (list :diagnostic :image image-box :button button-box
                                 :posn (posn-at-point pos window)
                                 :body (list (window-body-width window t)
                                             (window-body-height window t))
                                 :start (window-start window)))
                          (error "No measured image/button: %S" candidate))
                        (with-selected-window window
                          (goto-char second-pos)
                          (dv-layout--mouse (list 'mouse-1 (nth 4 button-box)))
                          (unless (= (plist-get dv-layout--last-activation :position) pos)
                            (error "Mouse selected wrong image"))
                          (goto-char pos)
                          (unless (eq (key-binding (kbd "RET")) #'dv-layout--ret)
                            (error "RET keymap does not resolve to prototype"))
                          (call-interactively #'dv-layout--ret)
                          (unless (= (plist-get dv-layout--last-activation :position) pos)
                            (error "RET selected wrong image")))
                        (dv-layout--report
                         (list :candidate candidate :size dimensions
                               :window (if (eq window left) 'left 'right)
                               :window-width (window-body-width window t)
                               :image image-box :button-hit (cl-subseq button-box 0 4)
                               :sample-glyph (dv-layout--object-box (nth 4 button-box))
                               :mouse-ret-target pos))))))
                (dv-layout--clear)
                (with-silent-modifications
                  (put-text-property pos (1+ pos) 'display (dv-layout--image 200 100)))
                (let ((overlay (dv-layout--attach pos left 'raised-after)))
                  (with-silent-modifications
                    (put-text-property pos (1+ pos) 'display (dv-layout--image 280 160)))
                  (force-window-update buffer)
                  (sit-for 0.05)
                  (redisplay t)
                  (dv-layout--report
                   (list :display-update-without-reattach
                         :cached-size (overlay-get overlay 'dv-layout-size)
                         :actual-image (dv-layout--image-box pos left)
                         :button-hit (cl-subseq (dv-layout--button-box overlay left) 0 4))))
                (dv-layout--clear)
                (dv-layout--report '(:non-destructive t :mouse-ret t))))
          (error (setq failures 1) (dv-layout--report (list :error err))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (dv-layout--clear))
        (kill-buffer buffer)))
    (kill-emacs failures)))

(ert-deftest dv-layout-fixture-inert-svg ()
  (should (string-match-p "<rect " (dv-layout--svg 200 100)))
  (should-not (string-match-p "href\\|script\\|foreignObject" (dv-layout--svg 200 100)))
  (should-error (dv-layout--svg -1 100)))

(ert-deftest dv-layout-decoration-preserves-state ()
  (with-temp-buffer
    (let* ((pos (dv-layout--fixture '(image :type svg :data "fixture")))
           (foreign (make-overlay pos (1+ pos)))
           (window (selected-window)))
      (setq buffer-undo-list nil)
      (goto-char pos)
      (narrow-to-region pos (1+ pos))
      (set-buffer-modified-p nil)
      (let ((before (dv-layout--snapshot)))
        (cl-letf (((symbol-function 'image-size) (lambda (&rest _) '(200 . 100))))
          (dolist (candidate '(plain-after raised-after aligned-after aligned-before))
            (dv-layout--attach pos window candidate)
            (should (equal before (dv-layout--snapshot)))
            (dv-layout--clear)
            (should (equal before (dv-layout--snapshot))))))
      (should (overlay-buffer foreign)))))

(ert-deftest dv-layout-missing-image-has-no-fallback ()
  (with-temp-buffer
    (let* ((pos (dv-layout--fixture))
           (overlay (dv-layout--attach pos (selected-window) 'aligned-after)))
      (should (dv-layout--valid-p overlay))
      (should-not (overlay-get overlay 'after-string))
      (should-not (overlay-get overlay 'before-string))
      (dv-layout--clear))))

(ert-deftest dv-layout-current-source-and-stale-rejection ()
  (save-window-excursion
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (let* ((pos (dv-layout--fixture))
             (overlay (dv-layout--attach pos (selected-window) 'plain-after)))
        (should (equal (plist-get (dv-layout--activate overlay (selected-window)) :source)
                       dv-layout--source))
        (put-text-property pos (1+ pos) 'md-render-source "changed")
        (should-error (dv-layout--activate overlay (selected-window)) :type 'user-error)
        (dv-layout--clear)
        (should-not (marker-buffer (overlay-get overlay 'dv-layout-target)))
        (should-error (dv-layout--activate overlay (selected-window)) :type 'user-error)))))

(ert-deftest dv-layout-adjacent-placeholder-identity ()
  (save-window-excursion
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (let* ((first (dv-layout--fixture))
             (second (dv-layout--fixture))
             (a (dv-layout--attach first (selected-window) 'plain-after))
             (b (dv-layout--attach second (selected-window) 'plain-after)))
        (should (dv-layout--valid-p a)) (should (dv-layout--valid-p b))
        (goto-char first)
        (should (= second (plist-get (dv-layout--activate b (selected-window)) :position)))
        (dv-layout--clear)))))

(ert-deftest dv-layout-window-local-and-ret ()
  (save-window-excursion
    (with-temp-buffer
      (set-window-buffer (selected-window) (current-buffer))
      (let* ((pos (dv-layout--fixture))
             (left (selected-window))
             (right (split-window-right))
             (a (dv-layout--attach pos left 'plain-after))
             (b (dv-layout--attach pos right 'plain-after)))
        (set-window-buffer right (current-buffer))
        (goto-char pos)
        (call-interactively #'dv-layout--ret)
        (should (eq left (plist-get dv-layout--last-activation :window)))
        (should-error (dv-layout--activate a right) :type 'user-error)
        (with-selected-window right
          (goto-char pos)
          (call-interactively #'dv-layout--ret)
          (should (eq right (plist-get dv-layout--last-activation :window))))
        (should (dv-layout--valid-p b))
        (dv-layout--clear)))))

(ert-deftest dv-layout-display-update-does-not-refresh-automatically ()
  (with-temp-buffer
    (let* ((pos (dv-layout--fixture '(image :type svg :data "one")))
           (size '(200 . 100)))
      (cl-letf (((symbol-function 'image-size) (lambda (&rest _) size)))
        (let ((overlay (dv-layout--attach pos (selected-window) 'raised-after)))
          (put-text-property pos (1+ pos) 'display '(image :type svg :data "two"))
          (setq size '(280 . 160))
          (should (equal (overlay-get overlay 'dv-layout-size) '(200 . 100)))
          (should (dv-layout--valid-p overlay))
          (dv-layout--clear)
          (let ((new (dv-layout--attach pos (selected-window) 'raised-after)))
            (should (equal (overlay-get new 'dv-layout-size) size)))))
      (dv-layout--clear))))

(ert-deftest dv-layout-source-return-evaporates ()
  (with-temp-buffer
    (let* ((pos (dv-layout--fixture))
           (overlay (dv-layout--attach pos (selected-window) 'plain-after)))
      (erase-buffer)
      (insert dv-layout--source)
      (should-not (overlay-buffer overlay))
      (should-not (dv-layout--valid-p overlay))
      (set-marker (overlay-get overlay 'dv-layout-target) nil))))

(declare-function md-render--insert-media "md-render-media")
(declare-function md-render-reconstruct "md-render-core")

(ert-deftest dv-layout-real-md-placeholder-contract ()
  (skip-unless (locate-library "md-render-media"))
  (require 'md-render-media)
  (with-temp-buffer
    (insert dv-layout--source dv-layout--source)
    (cl-letf (((symbol-function 'md-render--media-cache-file)
               (lambda (&rest _) "/fixture/never-read.png"))
              ((symbol-function 'md-render--watch-media)
               (lambda (_backend _source _file marker _label)
                 (set-marker marker nil))))
      (md-render--insert-media
       :start (point-min) :end (+ (point-min) (length dv-layout--source))
       :source dv-layout--source :render-source "graph TD; A-->B"
       :backend 'mermaid :block-p t :label "fixture")
      (md-render--insert-media
       :start (+ (point-min) 4) :end (point-max)
       :source dv-layout--source :render-source "graph TD; A-->B"
       :backend 'mermaid :block-p t :label "fixture"))
    (should (equal (buffer-string)
                   (propertize (concat dv-layout--placeholder dv-layout--placeholder)
                               'md-render-source dv-layout--source
                               'md-render-frozen t
                               'rear-nonsticky '(md-render-frozen md-render-source
                                                  md-render-media-file))))
    ;; Compare actual runs separately: media-file belongs only to image spaces.
    (should (equal (get-text-property 2 'md-render-media-file) "/fixture/never-read.png"))
    (should (equal (get-text-property 6 'md-render-media-file) "/fixture/never-read.png"))
    (let ((baseline (md-render-reconstruct (point-min) (point-max)))
          (a (dv-layout--attach 2 (selected-window) 'plain-after))
          (b (dv-layout--attach 6 (selected-window) 'plain-after)))
      (should (dv-layout--valid-p a)) (should (dv-layout--valid-p b))
      ;; Upstream coalesces equal adjacent source-runs: do not claim roundtrip
      ;; correctness for this synthetic adjacency or silently fix its text.
      (should (equal baseline dv-layout--source))
      (should (equal (md-render-reconstruct (point-min) (point-max)) baseline)))
    (dv-layout--clear)))

(provide 'diagram-viewer-layout-test)
;;; diagram-viewer-layout-test.el ends here
