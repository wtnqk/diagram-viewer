;;; diagram-viewer.el --- Asynchronous diagram viewing foundation -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: diagram-viewer contributors
;; Version: 0.1.0
;; Keywords: multimedia, tools

;;; Commentary:

;; Core types, registries, request scheduling and checked SVG publication.
;; No renderer/source adapter is loaded here.  SVG validation and the nonfile
;; image viewer are lazy Core helpers.  Load and register the desired adapters
;; explicitly; their source syntax and external commands stay outside Core.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup diagram-viewer nil
  "Asynchronous diagram viewing."
  :group 'multimedia)

(defcustom diagram-viewer-cache-directory
  (expand-file-name "diagram-viewer/cache/" user-emacs-directory)
  "Local directory reserved for diagram-viewer artifacts."
  :type 'directory)

(defcustom diagram-viewer-max-concurrent-jobs 2
  "Maximum number of simultaneously running renderer jobs."
  :type '(integer 1 *))

(defcustom diagram-viewer-default-renderer-alist nil
  "Preferred renderer IDs, indexed by language symbol."
  :type '(alist :key-type symbol :value-type symbol))

(defcustom diagram-viewer-log-tail-length 4000
  "Maximum number of characters retained in a diagnostic message."
  :type 'natnum)

(define-error 'diagram-viewer-error "Diagram viewer error")
(dolist (entry '((diagram-viewer-invalid-argument . "Invalid diagram argument")
                 (diagram-viewer-source-not-found . "No diagram source found")
                 (diagram-viewer-renderer-not-found . "No matching renderer")
                 (diagram-viewer-renderer-unavailable . "Renderer unavailable")
                 (diagram-viewer-process-failed . "Diagram rendering failed")
                 (diagram-viewer-invalid-output . "Invalid diagram output")
                 (diagram-viewer-storage-error . "Diagram storage failed")
                 (diagram-viewer-cancelled . "Diagram request cancelled")))
  (define-error (car entry) (cdr entry) 'diagram-viewer-error))

(cl-defstruct (diagram-viewer-source (:constructor diagram-viewer--source-create))
  text language origin-adapter-id buffer range options valid-p refresh-function adapter-data)

(cl-defun diagram-viewer-source-create
  (&key text language adapter-id buffer range options valid-p refresh-function adapter-data)
  "Create a source descriptor with TEXT, LANGUAGE and origin ADAPTER-ID.
BUFFER and RANGE identify the source.  OPTIONS are rendering inputs.
VALID-P and REFRESH-FUNCTION receive the owner's descriptor; ADAPTER-DATA
contains only plain immutable data.  Fields are validated on submission."
  (diagram-viewer--source-create
   :text text :language language :origin-adapter-id adapter-id :buffer buffer
   :range range :options options :valid-p valid-p :refresh-function refresh-function
   :adapter-data adapter-data))
(cl-defstruct (diagram-viewer-renderer (:constructor diagram-viewer-renderer-create))
  id languages output-extension available-p render-function cache-context-function cancel-function)
(cl-defstruct (diagram-viewer-source-adapter
               (:constructor diagram-viewer-source-adapter-create))
  id (priority 0) applicable-p source-function describe-function)
(cl-defstruct (diagram-viewer-request (:constructor diagram-viewer-request-create))
  id source language source-adapter-id source-buffer source-range options cache-key
  callback (state 'pending) renderer context execution-plan source-snapshot
  bypass-cache delivered-p result job submitting-p)
(cl-defstruct (diagram-viewer-result (:constructor diagram-viewer-result-create))
  request-id state artifact artifact-data cache-key cache-hit-p stale-p error)
(cl-defstruct (diagram-viewer--job (:constructor diagram-viewer--job-create))
  key generation renderer request (state 'queued) waiters handle handle-ready-p
  temporary-output counted-p)

(defvar diagram-viewer--renderers (make-hash-table :test #'eq))
(defvar diagram-viewer--source-adapters (make-hash-table :test #'eq))
(defvar diagram-viewer--jobs (make-hash-table :test #'equal))
(defvar diagram-viewer--stopping-jobs (make-hash-table :test #'equal))
(defvar diagram-viewer--requests (make-hash-table :test #'eql))
(defvar diagram-viewer--queue nil)
(defvar diagram-viewer--active-count 0)
(defvar diagram-viewer--next-id 0)
(defvar diagram-viewer--events nil)
(defvar diagram-viewer--event-timer nil)
(defvar diagram-viewer--pumping nil)
(defvar diagram-viewer--diagnostics nil)
(defvar diagram-viewer--next-diagnostic-id 0)

(defun diagram-viewer--invalid (message)
  "Signal an invalid argument with MESSAGE."
  (signal 'diagram-viewer-invalid-argument (list message)))

(defun diagram-viewer--id-p (value)
  "Return non-nil for a non-keyword, non-nil ID VALUE."
  (and (symbolp value) value (not (memq value '(t))) (not (keywordp value))))

(defun diagram-viewer--frame (tag bytes)
  "Encode TAG and length-prefixed BYTES without delimiter ambiguity."
  (concat tag (number-to-string (length bytes)) ":" bytes))

(defun diagram-viewer--canonical (value &optional ancestors)
  "Return a deterministic byte encoding of VALUE.
ANCESTORS detects recursive values.  Nested maps use (:map KEY VALUE ...)."
  (cond
   ((null value) "n0:")
   ((eq value t) "b1:1")
   ((integerp value) (diagram-viewer--frame "i" (number-to-string value)))
   ((stringp value)
    (diagram-viewer--frame "s" (encode-coding-string
				(substring-no-properties value) 'utf-8-unix t)))
   ((symbolp value)
    (unless (eq value (intern-soft (symbol-name value)))
      (diagram-viewer--invalid "Uninterned symbols are not canonical data"))
    (diagram-viewer--frame "y" (encode-coding-string (symbol-name value) 'utf-8-unix t)))
   ((and (consp value) (functionp value))
    (diagram-viewer--invalid "Functions are not canonical data"))
   ((consp value)
    (when (memq value ancestors) (diagram-viewer--invalid "Cyclic data"))
    (unless (proper-list-p value) (diagram-viewer--invalid "Expected a proper list"))
    (if (eq (car value) :map)
        (diagram-viewer--canonical-plist (cdr value) (cons value ancestors))
      (diagram-viewer--frame
       "l" (mapconcat (lambda (item)
                        (diagram-viewer--canonical item (cons value ancestors))) value ""))))
   (t (diagram-viewer--invalid "Unsupported canonical data type"))))

(defun diagram-viewer--canonical-plist (plist &optional ancestors)
  "Encode PLIST as a keyword map, rejecting duplicate keys.
ANCESTORS is the recursion detection stack."
  (unless (and (proper-list-p plist) (zerop (% (length plist) 2)))
    (diagram-viewer--invalid "Expected an even keyword plist"))
  (let ((tail plist) keys pairs)
    (while tail
      (let ((key (pop tail)) (value (pop tail)))
        (unless (and (keywordp key) (not (memq key keys)))
          (diagram-viewer--invalid "Invalid or repeated plist key"))
        (push key keys)
        (push (cons key value) pairs)))
    (setq pairs (sort pairs (lambda (a b) (string< (symbol-name (car a))
                                                   (symbol-name (car b))))))
    (diagram-viewer--frame
     "m" (mapconcat (lambda (pair)
                      (concat (diagram-viewer--canonical (car pair) ancestors)
                              (diagram-viewer--canonical (cdr pair) ancestors))) pairs ""))))

(defun diagram-viewer--copy-data (data)
  "Copy validated plain DATA, including its strings."
  (cond ((stringp data) (substring-no-properties data))
        ((consp data) (cons (diagram-viewer--copy-data (car data))
                            (diagram-viewer--copy-data (cdr data))))
        (t data)))

(defun diagram-viewer--copy-renderer (renderer)
  "Return an independent descriptor copied from RENDERER."
  (let ((copy (copy-diagram-viewer-renderer renderer)))
    (setf (diagram-viewer-renderer-languages copy)
          (copy-sequence (diagram-viewer-renderer-languages renderer))
          (diagram-viewer-renderer-output-extension copy)
          (copy-sequence (diagram-viewer-renderer-output-extension renderer)))
    copy))

(defun diagram-viewer-register-renderer (renderer &optional replace)
  "Register RENDERER; require explicit REPLACE for an existing ID."
  (unless (and (diagram-viewer-renderer-p renderer)
               (diagram-viewer--id-p (diagram-viewer-renderer-id renderer))
               (proper-list-p (diagram-viewer-renderer-languages renderer))
               (diagram-viewer-renderer-languages renderer)
               (cl-every #'diagram-viewer--id-p (diagram-viewer-renderer-languages renderer))
               (equal (diagram-viewer-renderer-output-extension renderer) ".svg")
               (functionp (diagram-viewer-renderer-available-p renderer))
               (functionp (diagram-viewer-renderer-render-function renderer))
               (functionp (diagram-viewer-renderer-cache-context-function renderer))
               (or (null (diagram-viewer-renderer-cancel-function renderer))
                   (functionp (diagram-viewer-renderer-cancel-function renderer))))
    (diagram-viewer--invalid "Invalid renderer descriptor (MVP supports .svg only)"))
  (let ((id (diagram-viewer-renderer-id renderer)))
    (when (and (gethash id diagram-viewer--renderers) (not replace))
      (diagram-viewer--invalid "Renderer ID is already registered"))
    (puthash id (diagram-viewer--copy-renderer renderer) diagram-viewer--renderers)
    id))

(defun diagram-viewer-unregister-renderer (renderer-id)
  "Remove RENDERER-ID from future selection, leaving existing jobs intact."
  (remhash renderer-id diagram-viewer--renderers))

(defun diagram-viewer-get-renderer (renderer-id)
  "Return a copy of RENDERER-ID's descriptor, or nil."
  (when-let* ((renderer (gethash renderer-id diagram-viewer--renderers)))
    (diagram-viewer--copy-renderer renderer)))

(defun diagram-viewer-register-source-adapter (adapter &optional replace)
  "Register source ADAPTER, allowing an existing ID only with REPLACE."
  (unless (and (diagram-viewer-source-adapter-p adapter)
               (diagram-viewer--id-p (diagram-viewer-source-adapter-id adapter))
               (integerp (diagram-viewer-source-adapter-priority adapter))
               (functionp (diagram-viewer-source-adapter-applicable-p adapter))
               (functionp (diagram-viewer-source-adapter-source-function adapter))
               (or (null (diagram-viewer-source-adapter-describe-function adapter))
                   (functionp (diagram-viewer-source-adapter-describe-function adapter))))
    (diagram-viewer--invalid "Invalid source adapter descriptor"))
  (let ((id (diagram-viewer-source-adapter-id adapter)))
    (when (and (gethash id diagram-viewer--source-adapters) (not replace))
      (diagram-viewer--invalid "Source adapter ID is already registered"))
    (puthash id (copy-diagram-viewer-source-adapter adapter) diagram-viewer--source-adapters)
    id))

(defun diagram-viewer-unregister-source-adapter (adapter-id)
  "Remove ADAPTER-ID from future source selection."
  (remhash adapter-id diagram-viewer--source-adapters))

(defun diagram-viewer-get-source-adapter (adapter-id)
  "Return an independent copy of ADAPTER-ID's descriptor, or nil."
  (when-let* ((adapter (gethash adapter-id diagram-viewer--source-adapters)))
    (copy-diagram-viewer-source-adapter adapter)))

(defun diagram-viewer-select-renderer (language &optional renderer-id)
  "Select a renderer for LANGUAGE, optionally requiring RENDERER-ID.
Ambiguity is an error; this noninteractive API never prompts."
  (let* ((id (or renderer-id (alist-get language diagram-viewer-default-renderer-alist)))
         (candidates (if id
                         (let ((renderer (diagram-viewer-get-renderer id)))
                           (and renderer (list renderer)))
                       (let (found)
                         (maphash (lambda (_ renderer)
                                    (when (memq language (diagram-viewer-renderer-languages renderer))
                                      (push (diagram-viewer--copy-renderer renderer) found)))
                                  diagram-viewer--renderers)
                         found))))
    (unless (and (= (length candidates) 1)
                 (memq language (diagram-viewer-renderer-languages (car candidates))))
      (signal 'diagram-viewer-renderer-not-found
              (list "Missing, incompatible or ambiguous renderer"
                    (sort (mapcar #'diagram-viewer-renderer-id candidates)
                          (lambda (a b) (string< (symbol-name a) (symbol-name b)))))))
    (car candidates)))

(defun diagram-viewer-source-at-point (&optional adapter-id choose)
  "Extract the current source, optionally requiring ADAPTER-ID.
CHOOSE t prompts for any applicable adapter; `on-ambiguity' prompts only
for tied priorities.  Nil never prompts."
  (let (candidates result)
    (maphash (lambda (id adapter)
               (when (and (or (not adapter-id) (eq id adapter-id))
                          (save-excursion
                            (funcall (diagram-viewer-source-adapter-applicable-p adapter))))
                 (push (copy-diagram-viewer-source-adapter adapter) candidates)))
             diagram-viewer--source-adapters)
    (setq candidates
          (sort candidates
                (lambda (a b)
                  (if (= (diagram-viewer-source-adapter-priority a)
                         (diagram-viewer-source-adapter-priority b))
                      (string< (symbol-name (diagram-viewer-source-adapter-id a))
                               (symbol-name (diagram-viewer-source-adapter-id b)))
                    (> (diagram-viewer-source-adapter-priority a)
                       (diagram-viewer-source-adapter-priority b))))))
    (while (and candidates (not result))
      (let* ((priority (diagram-viewer-source-adapter-priority (car candidates)))
             (ties (cl-remove-if-not
                    (lambda (a) (= priority (diagram-viewer-source-adapter-priority a)))
                    candidates))
             (pool (if (eq choose t) candidates ties))
             (selected
              (if (and (not adapter-id) (or (eq choose t) (> (length ties) 1)))
                  (if (not choose)
                      (signal 'diagram-viewer-source-not-found (list "Ambiguous source adapter"))
                    (let* ((names (mapcar
                                   (lambda (a) (symbol-name (diagram-viewer-source-adapter-id a)))
                                   pool))
                           (name (completing-read "Source adapter: " names nil t)))
                      (cl-find name pool :key
                               (lambda (a) (symbol-name (diagram-viewer-source-adapter-id a)))
                               :test #'equal)))
                (car candidates))))
        (setq candidates (delq selected candidates))
        (condition-case nil
            (setq result (save-excursion
                           (funcall (diagram-viewer-source-adapter-source-function selected))))
          (diagram-viewer-source-not-found nil))))
    (unless result (signal 'diagram-viewer-source-not-found nil))
    (unless (diagram-viewer-source-p result)
      (diagram-viewer--invalid "Adapter did not return a source descriptor"))
    result))

(defun diagram-viewer--check-source (source)
  "Validate SOURCE before allocating request resources."
  (unless (and (diagram-viewer-source-p source)
               (stringp (diagram-viewer-source-text source))
               (diagram-viewer--id-p (diagram-viewer-source-language source))
               (or (null (diagram-viewer-source-valid-p source))
                   (functionp (diagram-viewer-source-valid-p source)))
               (or (null (diagram-viewer-source-refresh-function source))
                   (functionp (diagram-viewer-source-refresh-function source))))
    (diagram-viewer--invalid "Invalid source descriptor"))
  (let ((range (diagram-viewer-source-range source))
        (buffer (diagram-viewer-source-buffer source)))
    (unless (and (buffer-live-p buffer) (consp range)
                 (markerp (car range)) (markerp (cdr range))
                 (eq (marker-buffer (car range)) buffer)
                 (eq (marker-buffer (cdr range)) buffer)
                 (< (marker-position (car range)) (marker-position (cdr range))))
      (diagram-viewer--invalid "Source needs a live, nonempty marker range")))
  (diagram-viewer--canonical-plist (diagram-viewer-source-options source))
  (diagram-viewer--canonical (diagram-viewer-source-adapter-data source)))

(defun diagram-viewer--copy-source (source)
  "Clone SOURCE, its data and both markers for a separate owner."
  (diagram-viewer--check-source source)
  (let ((copy (copy-diagram-viewer-source source))
        (range (diagram-viewer-source-range source)))
    (setf (diagram-viewer-source-text copy)
          (substring-no-properties (diagram-viewer-source-text source))
          (diagram-viewer-source-range copy)
          (cons (copy-marker (car range) nil) (copy-marker (cdr range) t))
          (diagram-viewer-source-options copy)
          (diagram-viewer--copy-data (diagram-viewer-source-options source))
          (diagram-viewer-source-adapter-data copy)
          (diagram-viewer--copy-data (diagram-viewer-source-adapter-data source)))
    copy))

(defun diagram-viewer--release-source (source)
  "Detach only the markers owned by SOURCE.  Repeated release is safe."
  (when (diagram-viewer-source-p source)
    (let ((range (diagram-viewer-source-range source)))
      (when (consp range)
        (when (markerp (car range)) (set-marker (car range) nil))
        (when (markerp (cdr range)) (set-marker (cdr range) nil))))))

(defun diagram-viewer--range-text (source)
  "Return the current property-free range text for SOURCE."
  (with-current-buffer (diagram-viewer-source-buffer source)
    (save-restriction
      (widen)
      (buffer-substring-no-properties (car (diagram-viewer-source-range source))
                                      (cdr (diagram-viewer-source-range source))))))

(defun diagram-viewer--source-current-p (source snapshot)
  "Check SOURCE's markers, SNAPSHOT text and adapter-specific validity."
  (condition-case nil
      (let ((range (diagram-viewer-source-range source))
            (buffer (diagram-viewer-source-buffer source)))
        (and (buffer-live-p buffer)
             (eq (marker-buffer (car range)) buffer)
             (eq (marker-buffer (cdr range)) buffer)
             (< (marker-position (car range)) (marker-position (cdr range)))
             (equal snapshot (diagram-viewer--range-text source))
             (or (null (diagram-viewer-source-valid-p source))
                 (condition-case err
                     (funcall (diagram-viewer-source-valid-p source) source)
                   ((error quit) (diagram-viewer--diagnose err) nil)))))
    (error nil)))

(defun diagram-viewer--cache-key (request)
  "Return the SHA-256 key of REQUEST's immutable rendering input."
  (secure-hash
   'sha256
   (concat (diagram-viewer--canonical '(2))
           (diagram-viewer--canonical
            (diagram-viewer-renderer-id (diagram-viewer-request-renderer request)))
           (diagram-viewer--canonical ".svg")
           (diagram-viewer--canonical (diagram-viewer-request-source request))
           (diagram-viewer--canonical-plist (diagram-viewer-request-options request))
           (diagram-viewer--canonical-plist (diagram-viewer-request-context request)))))

(defun diagram-viewer--cache-path (key)
  "Return the dedicated cache path for validated KEY."
  (unless (and (stringp key) (string-match-p "\\`[a-f0-9]\\{64\\}\\'" key))
    (diagram-viewer--invalid "Invalid cache key"))
  (let ((path (expand-file-name (concat key ".svg") diagram-viewer-cache-directory)))
    (when (file-remote-p path)
      (signal 'diagram-viewer-storage-error '("Remote caches are unsupported")))
    path))

(declare-function diagram-viewer-svg-validate-file "diagram-viewer-svg" (path))

(defun diagram-viewer--validate-output (path)
  "Return a checked and normalized passive SVG read from PATH."
  (require 'diagram-viewer-svg)
  (diagram-viewer-svg-validate-file path))

(defun diagram-viewer--read-cache (key)
  "Read and validate cached KEY, removing invalid entries only."
  (let ((path (diagram-viewer--cache-path key)))
    (cond
     ((file-symlink-p path) (delete-file path) nil)
     ((file-exists-p path)
      (condition-case err
          (cons path (diagram-viewer--validate-output path))
        (diagram-viewer-invalid-output
         (if (file-directory-p path)
             (signal 'diagram-viewer-storage-error '("Cache entry is a directory"))
           (delete-file path))
         nil)
        (file-error (signal 'diagram-viewer-storage-error (cdr err))))))))

(defun diagram-viewer--record-diagnostic (error-data)
  "Copy ERROR-DATA into the bounded diagnostic history and return it.
Only supported scalar metadata and bounded message/stream tails are retained."
  (unless (and (proper-list-p error-data) (zerop (% (length error-data) 2)))
    (setq error-data '(:condition diagram-viewer-process-failed
                                  :message "Malformed renderer error details")))
  (let* ((limit (if (natnump diagram-viewer-log-tail-length)
                    diagram-viewer-log-tail-length 4000))
         (data (list :id (cl-incf diagram-viewer--next-diagnostic-id)
                     :condition (if (symbolp (plist-get error-data :condition))
                                    (or (plist-get error-data :condition) 'diagram-viewer-process-failed)
                                  'diagram-viewer-process-failed))))
    (dolist (key '(:message :stdout :stderr))
      (let ((text (plist-get error-data key)))
        (when (stringp text)
          (setq data (plist-put data key
                                (substring-no-properties text (max 0 (- (length text) limit))))))))
    (unless (plist-get data :message)
      (setq data (plist-put data :message "Diagram operation failed")))
    (dolist (key '(:reason :exit-code :signal))
      (let ((value (plist-get error-data key)))
        (when (or (symbolp value) (integerp value))
          (setq data (plist-put data key value)))))
    (push data diagram-viewer--diagnostics)
    (when (> (length diagram-viewer--diagnostics) 20)
      (setcdr (nthcdr 19 diagram-viewer--diagnostics) nil))
    data))

(defun diagram-viewer--diagnose (err)
  "Record a bounded diagnostic for ERR and return structured error data."
  (diagram-viewer--record-diagnostic
   (list :condition (if (and (consp err) (symbolp (car err)))
                        (car err) 'diagram-viewer-process-failed)
         :message (condition-case nil (error-message-string err) (error "Adapter error")))))

(defun diagram-viewer--defer (function &rest args)
  "Queue FUNCTION with ARGS for the next event turn."
  (setq diagram-viewer--events
        (nconc diagram-viewer--events (list (cons function args))))
  (unless diagram-viewer--event-timer
    (setq diagram-viewer--event-timer (run-at-time 0 nil #'diagram-viewer--drain-events))))

(defun diagram-viewer--drain-events ()
  "Dispatch queued events without waiting for timers."
  (when (timerp diagram-viewer--event-timer)
    (cancel-timer diagram-viewer--event-timer))
  (setq diagram-viewer--event-timer nil)
  (while diagram-viewer--events
    (let ((event (pop diagram-viewer--events)))
      (condition-case err (apply (car event) (cdr event))
        ((error quit) (diagram-viewer--diagnose err))))))

(defun diagram-viewer--deliver (request)
  "Deliver REQUEST's terminal result at most once, then release its markers."
  (unless (diagram-viewer-request-delivered-p request)
    (setf (diagram-viewer-request-delivered-p request) t)
    (let* ((snapshot (diagram-viewer-request-source-snapshot request))
           (source (car snapshot))
           (result (diagram-viewer-request-result request)))
      (setf (diagram-viewer-result-stale-p result)
            (not (diagram-viewer--source-current-p source (cdr snapshot))))
      (unwind-protect
          (condition-case err
              (funcall (diagram-viewer-request-callback request) result)
            ((error quit) (diagram-viewer--diagnose err)))
        (diagram-viewer--release-source source)
        (remhash (diagram-viewer-request-id request) diagram-viewer--requests)))))

(defun diagram-viewer--finish-request (request state &optional error-data artifact bytes hit)
  "Finish REQUEST with STATE, ERROR-DATA, ARTIFACT, BYTES and HIT metadata."
  (unless (memq (diagram-viewer-request-state request) '(succeeded failed cancelled))
    (when (and error-data (not (memq error-data diagram-viewer--diagnostics)))
      (setq error-data (diagram-viewer--record-diagnostic error-data)))
    (setf (diagram-viewer-request-state request) state
          (diagram-viewer-request-result request)
          (diagram-viewer-result-create
           :request-id (diagram-viewer-request-id request) :state state
           :error error-data :artifact artifact :artifact-data bytes
           :cache-key (diagram-viewer-request-cache-key request) :cache-hit-p hit)
          (diagram-viewer-request-job request) nil)
    (unless (diagram-viewer-request-submitting-p request)
      (diagram-viewer--defer #'diagram-viewer--deliver request))))

(defun diagram-viewer--cancel-source-requests ()
  "Cancel requests belonging to the buffer being killed."
  (let ((buffer (current-buffer)) requests)
    (maphash (lambda (_ request)
               (when (eq (diagram-viewer-request-source-buffer request) buffer)
                 (push request requests))) diagram-viewer--requests)
    (mapc #'diagram-viewer-cancel requests)))

(cl-defun diagram-viewer-submit (source &key renderer callback bypass-cache)
  "Submit SOURCE to RENDERER and return a request before invoking CALLBACK.
CALLBACK receives one result.  BYPASS-CACHE skips stored artifacts, not jobs.
Contract violations signal synchronously; accepted operational errors are
reported asynchronously.  Only the restricted passive SVG subset is accepted."
  (diagram-viewer--check-source source)
  (unless (or (null callback) (functionp callback))
    (diagram-viewer--invalid "Callback must be callable"))
  (unless (and (integerp diagram-viewer-max-concurrent-jobs)
	       (> diagram-viewer-max-concurrent-jobs 0))
    (diagram-viewer--invalid "Concurrency limit must be a positive integer"))
  (let* ((copy (diagram-viewer--copy-source source))
	 (request (diagram-viewer-request-create
		   :id (cl-incf diagram-viewer--next-id)
		   :source (diagram-viewer-source-text copy)
		   :language (diagram-viewer-source-language copy)
		   :source-adapter-id (diagram-viewer-source-origin-adapter-id copy)
		   :source-buffer (diagram-viewer-source-buffer copy)
		   :source-range (diagram-viewer-source-range copy)
		   :options (diagram-viewer-source-options copy)
		   :source-snapshot (cons copy (diagram-viewer--range-text copy))
		   :callback (or callback #'ignore) :bypass-cache bypass-cache
		   :submitting-p t)))
    (puthash (diagram-viewer-request-id request) request diagram-viewer--requests)
    (with-current-buffer (diagram-viewer-source-buffer copy)
      (add-hook 'kill-buffer-hook #'diagram-viewer--cancel-source-requests nil t))
    (unwind-protect
	(condition-case err
	    (let* ((selected (diagram-viewer-select-renderer
			      (diagram-viewer-request-language request) renderer))
		   (availability (funcall (diagram-viewer-renderer-available-p selected))))
	      (unless (eq (diagram-viewer-request-state request) 'pending)
		(cl-return-from diagram-viewer-submit request))
	      (unless (eq availability t)
		(signal 'diagram-viewer-renderer-unavailable
			(list (if (and (listp availability) (plist-get availability :message))
				  (plist-get availability :message)
				"Invalid availability response"))))
	      (let ((plan (funcall (diagram-viewer-renderer-cache-context-function selected)
				   (diagram-viewer--copy-data (diagram-viewer-request-options request)))))
		(unless (eq (diagram-viewer-request-state request) 'pending)
		  (cl-return-from diagram-viewer-submit request))
		(diagram-viewer--canonical-plist plan)
		(diagram-viewer--canonical-plist (plist-get plan :context))
		(setf (diagram-viewer-request-renderer request) selected
		      (diagram-viewer-request-context request)
		      (diagram-viewer--copy-data (plist-get plan :context))
		      (diagram-viewer-request-execution-plan request)
		      (diagram-viewer--copy-data (plist-get plan :execution-plan))))
	      (setf (diagram-viewer-request-cache-key request) (diagram-viewer--cache-key request))
	      (let* ((key (diagram-viewer-request-cache-key request))
		     (cached (unless bypass-cache (diagram-viewer--read-cache key)))
		     (job (gethash key diagram-viewer--jobs)))
		(if cached
		    (diagram-viewer--finish-request request 'succeeded nil (car cached) (cdr cached) t)
		  (unless job
		    (setq job (diagram-viewer--job-create
			       :key key :generation (make-symbol "job") :renderer selected :request request))
		    (puthash key job diagram-viewer--jobs)
		    (setq diagram-viewer--queue (nconc diagram-viewer--queue (list job))))
		  (push request (diagram-viewer--job-waiters job))
		  (setf (diagram-viewer-request-job request) job)
		  (when (eq (diagram-viewer--job-state job) 'running)
		    (setf (diagram-viewer-request-state request) 'running))
		  (diagram-viewer--pump))))
	  (error (diagram-viewer--finish-request
		  request 'failed (diagram-viewer--diagnose
				   (if (eq (car err) 'file-error)
				       (cons 'diagram-viewer-storage-error (cdr err)) err)))))
      (setf (diagram-viewer-request-submitting-p request) nil)
      (when (memq (diagram-viewer-request-state request) '(succeeded failed cancelled))
	(diagram-viewer--defer #'diagram-viewer--deliver request)))
    request))

(defun diagram-viewer--pump ()
  "Start queued jobs while slots and per-key exclusion allow."
  (unless diagram-viewer--pumping
    (let ((diagram-viewer--pumping t) next)
      (while (and (< diagram-viewer--active-count diagram-viewer-max-concurrent-jobs)
                  (setq next (cl-find-if
                              (lambda (job)
                                (not (gethash (diagram-viewer--job-key job)
                                              diagram-viewer--stopping-jobs)))
                              diagram-viewer--queue)))
        (setq diagram-viewer--queue (delq next diagram-viewer--queue))
        (diagram-viewer--start-job next)))))

(defun diagram-viewer--start-job (job)
  "Acquire a slot and start JOB, handling synchronous completion."
  (setf (diagram-viewer--job-state job) 'running
        (diagram-viewer--job-counted-p job) t)
  (cl-incf diagram-viewer--active-count)
  (dolist (request (diagram-viewer--job-waiters job))
    (setf (diagram-viewer-request-state request) 'running))
  (condition-case err
      (progn
        (when (file-remote-p diagram-viewer-cache-directory)
          (signal 'diagram-viewer-storage-error '("Remote caches are unsupported")))
        (unless (file-directory-p diagram-viewer-cache-directory)
          (make-directory diagram-viewer-cache-directory t)
          (set-file-modes diagram-viewer-cache-directory #o700))
        (setf (diagram-viewer--job-temporary-output job)
              (make-temp-file (expand-file-name ".diagram-viewer-" diagram-viewer-cache-directory)
                              nil ".svg"))
        (let ((handle (funcall
                       (diagram-viewer-renderer-render-function (diagram-viewer--job-renderer job))
                       (diagram-viewer--job-request job) (diagram-viewer--job-temporary-output job)
                       (lambda (outcome) (diagram-viewer--complete-job job outcome)))))
          (when (memq (diagram-viewer--job-state job) '(running cancelling))
            (setf (diagram-viewer--job-handle job) handle
                  (diagram-viewer--job-handle-ready-p job) t)
            (when (eq (diagram-viewer--job-state job) 'cancelling)
              (diagram-viewer--stop-job job)))))
    (error (diagram-viewer--complete-job
            job (list :status 'failed :error (diagram-viewer--diagnose
                                              (if (eq (car err) 'file-error)
                                                  (cons 'diagram-viewer-storage-error (cdr err)) err)))))))

(defun diagram-viewer--complete-job (job outcome)
  "Finalize JOB once using OUTCOME, ignoring obsolete completions."
  (when (memq (diagram-viewer--job-state job) '(running cancelling))
    (unless (and (proper-list-p outcome) (zerop (% (length outcome) 2)))
      (setq outcome '(:status failed :error
			      (:condition diagram-viewer-process-failed
					  :message "Malformed renderer completion"))))
    (let ((state (if (eq (diagram-viewer--job-state job) 'cancelling) 'cancelled
                   (plist-get outcome :status)))
          (error-data (plist-get outcome :error)) artifact bytes)
      ;; Set a transient non-running state before calling validation code.
      (setf (diagram-viewer--job-state job) 'finishing)
      (unless (memq state '(succeeded failed cancelled))
        (setq state 'failed error-data
              (diagram-viewer--diagnose '(diagram-viewer-process-failed "Invalid completion status"))))
      (when (eq state 'succeeded)
        (condition-case err
            (progn
              (setq bytes (diagram-viewer--validate-output
                           (diagram-viewer--job-temporary-output job))
                    artifact (diagram-viewer--cache-path (diagram-viewer--job-key job)))
              (let ((coding-system-for-write 'no-conversion))
                (with-temp-buffer
                  (set-buffer-multibyte nil)
                  (insert bytes)
                  (write-region (point-min) (point-max)
                                (diagram-viewer--job-temporary-output job) nil 'silent)))
              (rename-file (diagram-viewer--job-temporary-output job) artifact t))
          (error (setq state 'failed artifact nil bytes nil error-data
                       (diagram-viewer--diagnose
                        (if (eq (car err) 'file-error)
                            (cons 'diagram-viewer-storage-error (cdr err)) err))))))
      (unless (or (eq state 'succeeded) error-data)
        (setq error-data (list :condition (if (eq state 'cancelled)
                                              'diagram-viewer-cancelled 'diagram-viewer-process-failed)
                               :message "Renderer did not provide error details")))
      (when (and error-data (not (memq error-data diagram-viewer--diagnostics)))
        (setq error-data (diagram-viewer--record-diagnostic error-data)))
      (setf (diagram-viewer--job-state job) state)
      (diagram-viewer--retire-job job)
      (dolist (request (diagram-viewer--job-waiters job))
        (diagram-viewer--finish-request request state error-data artifact bytes))
      (setf (diagram-viewer--job-waiters job) nil)
      (diagram-viewer--defer #'diagram-viewer--pump))))

(defun diagram-viewer--retire-job (job)
  "Remove JOB's exact identity, release its slot and clean its output."
  (let ((key (diagram-viewer--job-key job)))
    (when (eq job (gethash key diagram-viewer--jobs)) (remhash key diagram-viewer--jobs))
    (when (eq job (gethash key diagram-viewer--stopping-jobs))
      (remhash key diagram-viewer--stopping-jobs)))
  (setq diagram-viewer--queue (delq job diagram-viewer--queue))
  (when (diagram-viewer--job-counted-p job)
    (setf (diagram-viewer--job-counted-p job) nil)
    (cl-decf diagram-viewer--active-count))
  (when-let* ((path (diagram-viewer--job-temporary-output job)))
    (condition-case err
        (when (or (file-exists-p path) (file-symlink-p path)) (delete-file path))
      (file-error (diagram-viewer--diagnose err)))))

(defun diagram-viewer--stop-job (job)
  "Ask JOB's renderer to stop using the now-ready handle."
  (when (diagram-viewer--job-handle-ready-p job)
    (condition-case err
        (funcall (diagram-viewer-renderer-cancel-function (diagram-viewer--job-renderer job))
                 (diagram-viewer--job-handle job))
      ;; A faulty adapter may still be running: retain exclusion and the slot.
      ((error quit) (diagram-viewer--diagnose err)))))

(defun diagram-viewer-cancel (request)
  "Cancel REQUEST and stop its cancellable job when it has no waiters."
  (unless (diagram-viewer-request-p request) (diagram-viewer--invalid "Expected a request"))
  (unless (memq (diagram-viewer-request-state request) '(succeeded failed cancelled))
    (let ((job (diagram-viewer-request-job request)))
      (diagram-viewer--finish-request
       request 'cancelled '(:condition diagram-viewer-cancelled :message "Request cancelled"))
      (when job
        (setf (diagram-viewer--job-waiters job) (delq request (diagram-viewer--job-waiters job)))
        (unless (diagram-viewer--job-waiters job)
          (pcase (diagram-viewer--job-state job)
            ('queued
             (setf (diagram-viewer--job-state job) 'cancelled)
             (diagram-viewer--retire-job job))
            ('running
             (when (diagram-viewer-renderer-cancel-function (diagram-viewer--job-renderer job))
               (setf (diagram-viewer--job-state job) 'cancelling)
               (let ((key (diagram-viewer--job-key job)))
                 (when (eq job (gethash key diagram-viewer--jobs))
                   (remhash key diagram-viewer--jobs))
                 (puthash key job diagram-viewer--stopping-jobs))
               (diagram-viewer--stop-job job))))))))
  request)

(declare-function diagram-viewer--view "diagram-viewer-viewer"
                  (source &rest keys))
(declare-function diagram-viewer--show-log "diagram-viewer-viewer" (&optional diagnostic-id))

(cl-defun diagram-viewer-view (source &key renderer bypass-cache)
  "Display SOURCE with RENDERER in a dedicated viewer buffer.
Return the viewer buffer.  BYPASS-CACHE skips stored artifacts, not jobs.
SOURCE remains owned by its caller; the viewer and request take copies."
  (require 'diagram-viewer-viewer)
  (diagram-viewer--view source :renderer renderer :bypass-cache bypass-cache))

(defun diagram-viewer--choose-renderer (language)
  "Choose a renderer ID for LANGUAGE in an interactive command."
  (or (alist-get language diagram-viewer-default-renderer-alist)
      (let (ids)
        (maphash (lambda (id renderer)
                   (when (memq language (diagram-viewer-renderer-languages renderer))
                     (push id ids))) diagram-viewer--renderers)
        (when (> (length ids) 1)
          (intern (completing-read "Renderer: "
                                   (sort (mapcar #'symbol-name ids) #'string<) nil t))))))

;;;###autoload
(defun diagram-viewer-view-at-point (&optional choose-adapter)
  "Open the diagram at point in its dedicated viewer.
With prefix CHOOSE-ADAPTER, choose between applicable source adapters.
Otherwise prompt only for tied priorities.  Noninteractive calls never
prompt unless CHOOSE-ADAPTER explicitly requests source adapter selection."
  (interactive "P")
  (let ((interactivep (called-interactively-p 'interactive)) source)
    (unwind-protect
        (condition-case err
            (progn
              (setq source (diagram-viewer-source-at-point
                            nil (cond (choose-adapter t) (interactivep 'on-ambiguity))))
              (diagram-viewer-view source
                                   :renderer (when interactivep
                                               (diagram-viewer--choose-renderer
                                                (diagram-viewer-source-language source)))))
          (diagram-viewer-error
           (if interactivep (user-error "%s" (error-message-string err))
             (signal (car err) (cdr err)))))
      (when source (diagram-viewer--release-source source)))))

;;;###autoload
(defun diagram-viewer-show-log (&optional diagnostic-id)
  "Display bounded diagnostics, optionally selecting DIAGNOSTIC-ID."
  (interactive)
  (require 'diagram-viewer-viewer)
  (diagram-viewer--show-log diagnostic-id))

(defun diagram-viewer-cache-clear (&optional current-only)
  "Delete dedicated cache entries, or only the displayed entry with CURRENT-ONLY.
Interactively, a prefix selects the current item.  Confirm full deletion.
Only SHA-256-named SVG entries in the local dedicated directory are deleted;
never recursively remove directories.  Running jobs may recreate entries."
  (interactive "P")
  (let* ((directory (directory-file-name (expand-file-name diagram-viewer-cache-directory)))
         (key (and current-only (derived-mode-p 'diagram-viewer-mode)
                   (boundp 'diagram-viewer--viewer-cache-key)
                   (symbol-value 'diagram-viewer--viewer-cache-key)))
         (count 0))
    (when (or (file-remote-p directory) (file-symlink-p directory))
      (signal 'diagram-viewer-storage-error '("Cache must be a local non-symlink directory")))
    (when (and current-only (not key))
      (user-error "No displayed diagram cache entry"))
    (when (and (called-interactively-p 'interactive) (not current-only)
               (not (yes-or-no-p "Delete all diagram-viewer SVG cache entries? ")))
      (user-error "Cache deletion cancelled"))
    (condition-case err
        (let ((files (cond (current-only (list (diagram-viewer--cache-path key)))
                           ((file-directory-p directory)
                            (directory-files directory t "\\`[a-f0-9]\\{64\\}\\.svg\\'")))))
          (dolist (path files)
            (when (or (file-symlink-p path) (file-regular-p path))
              (delete-file path)
              (cl-incf count))))
      (file-error (signal 'diagram-viewer-storage-error (cdr err))))
    (when (called-interactively-p 'interactive)
      (message "Deleted %d SVG cache entries; running jobs may recreate them" count))
    count))

(provide 'diagram-viewer)
;;; diagram-viewer.el ends here
