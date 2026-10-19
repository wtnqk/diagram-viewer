;;; diagram-viewer-adapter-mermaid.el --- Async Mermaid renderer -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; Author: diagram-viewer contributors
;; Version: 0.1.0
;; Keywords: multimedia, tools

;;; Commentary:

;; Load without external dependencies, then call `diagram-viewer-mermaid-enable'.
;; Only the child environment is modified.  This is not an OS/browser sandbox.
;; Extra arguments are deliberately unsupported in this initial implementation.
;; Core owns output validation, publication and removal, including failed output.

;;; Code:

(require 'cl-lib)
(require 'diagram-viewer)

(defgroup diagram-viewer-mermaid nil
  "Mermaid command renderer."
  :group 'diagram-viewer)

(defcustom diagram-viewer-mermaid-command "mmdc"
  "Local Mermaid CLI executable name or absolute path."
  :type 'string)
(defcustom diagram-viewer-mermaid-browser-executable nil
  "Optional absolute executable browser path, set only in child environment."
  :type '(choice (const nil) file))
(defcustom diagram-viewer-mermaid-theme "default"
  "Default Mermaid theme."
  :type '(choice (const "default") (const "dark") (const "forest") (const "neutral")))
(defcustom diagram-viewer-mermaid-background-color "transparent"
  "Default background: transparent, a simple color name, or hexadecimal RGB."
  :type 'string)
(defcustom diagram-viewer-mermaid-extra-arguments nil
  "Extra CLI arguments; the initial empty allowlist rejects all arguments."
  :type '(repeat string))
(defcustom diagram-viewer-mermaid-timeout 120
  "Positive integer maximum running seconds, excluding Core queue time."
  :type '(integer 1 *))
(defcustom diagram-viewer-mermaid-log-limit 65536
  "Maximum bytes retained separately for stdout and stderr."
  :type 'natnum)

(defconst diagram-viewer-mermaid--config
  (concat "{\"htmlLabels\":false,\"flowchart\":{\"htmlLabels\":false},"
          "\"securityLevel\":\"strict\",\"secure\":[\"secure\",\"securityLevel\","
          "\"startOnLoad\",\"maxTextSize\",\"suppressErrorRendering\","
          "\"maxEdges\",\"htmlLabels\",\"flowchart\"]}")
  "Internal configuration, protected from source init directive overrides.")

(cl-defstruct (diagram-viewer-mermaid--handle
               (:constructor diagram-viewer-mermaid--handle-create))
  process stderr-process timer input config stdout stderr complete limit
  finished reason exit-status)

(defun diagram-viewer-mermaid--command ()
  "Resolve the configured command locally, without starting a process."
  (let ((command diagram-viewer-mermaid-command))
    (unless (and (stringp command) (not (file-remote-p command)))
      (signal 'diagram-viewer-renderer-unavailable '("Expected a local mmdc command")))
    (let ((path (executable-find command)))
      (unless (and path (file-regular-p path) (file-executable-p path))
        (signal 'diagram-viewer-renderer-unavailable '("mmdc executable not found")))
      (expand-file-name path))))

(defun diagram-viewer-mermaid--browser ()
  "Validate and return an independent optional browser path."
  (let ((browser diagram-viewer-mermaid-browser-executable))
    (when browser
      (unless (and (stringp browser) (file-name-absolute-p browser)
                   (not (file-remote-p browser)) (file-regular-p browser)
                   (file-executable-p browser))
        (signal 'diagram-viewer-renderer-unavailable
                '("Browser must be an absolute local executable file")))
      (substring-no-properties browser))))

(defun diagram-viewer-mermaid-available-p ()
  "Return t or an availability error plist, without invoking mmdc."
  (condition-case err
      (progn (diagram-viewer-mermaid--command)
             (diagram-viewer-mermaid--browser) t)
    (error (list :condition 'diagram-viewer-renderer-unavailable
                 :message (error-message-string err)))))

(defun diagram-viewer-mermaid-cache-context (options)
  "Snapshot OPTIONS and current settings without launching a process.
Supported keys are :theme, :background and :extra-arguments.  All extra
arguments are rejected until a tested, explicit allowlist is introduced."
  (diagram-viewer--canonical-plist options)
  (let ((tail options))
    (while tail
      (unless (memq (pop tail) '(:theme :background :extra-arguments))
        (signal 'diagram-viewer-invalid-argument '("Unknown Mermaid option")))
      (pop tail)))
  (let ((theme (if (plist-member options :theme) (plist-get options :theme)
                 diagram-viewer-mermaid-theme))
        (background (if (plist-member options :background) (plist-get options :background)
                      diagram-viewer-mermaid-background-color))
        (extra (if (plist-member options :extra-arguments) (plist-get options :extra-arguments)
                 diagram-viewer-mermaid-extra-arguments)))
    (unless (member theme '("default" "dark" "forest" "neutral"))
      (signal 'diagram-viewer-invalid-argument '("Unsupported Mermaid theme")))
    (unless (and (stringp background)
                 (string-match-p
                  "\\`\\(?:[A-Za-z]+\\|#[[:xdigit:]]\\{3\\}\\|#[[:xdigit:]]\\{6\\}\\|#[[:xdigit:]]\\{8\\}\\)\\'"
                  background))
      (signal 'diagram-viewer-invalid-argument '("Unsupported background color")))
    (unless (null extra)
      (signal 'diagram-viewer-invalid-argument '("Extra arguments are not allowlisted")))
    (unless (and (integerp diagram-viewer-mermaid-timeout)
                 (> diagram-viewer-mermaid-timeout 0)
                 (integerp diagram-viewer-mermaid-log-limit)
                 (>= diagram-viewer-mermaid-log-limit 0))
      (signal 'diagram-viewer-invalid-argument '("Invalid timeout or log limit")))
    (list :context
          (list :implementation 1 :theme (copy-sequence theme)
                :background (copy-sequence background) :extra-arguments nil
                :config (copy-sequence diagram-viewer-mermaid--config))
          :execution-plan
          (list :command (diagram-viewer-mermaid--command)
                :browser (diagram-viewer-mermaid--browser)
                :timeout diagram-viewer-mermaid-timeout
                :log-limit diagram-viewer-mermaid-log-limit))))

(defun diagram-viewer-mermaid--filter (handle stream chunk)
  "Append binary CHUNK for STREAM in HANDLE, retaining a bounded tail."
  (unless (diagram-viewer-mermaid--handle-finished handle)
    (let* ((old (if (eq stream 'stdout)
                    (diagram-viewer-mermaid--handle-stdout handle)
                  (diagram-viewer-mermaid--handle-stderr handle)))
           (bytes (if (multibyte-string-p chunk)
                      (encode-coding-string chunk 'utf-8-unix t) chunk))
           (limit (diagram-viewer-mermaid--handle-limit handle))
           (text (if (>= (length bytes) limit)
                     (substring bytes (- (length bytes) limit))
                   (concat (substring old (max 0 (- (length old) (- limit (length bytes)))))
                           bytes))))
      (if (eq stream 'stdout)
          (setf (diagram-viewer-mermaid--handle-stdout handle) text)
        (setf (diagram-viewer-mermaid--handle-stderr handle) text)))))

(defun diagram-viewer-mermaid--cleanup (handle)
  "Release only Adapter-owned resources belonging to HANDLE."
  (when (timerp (diagram-viewer-mermaid--handle-timer handle))
    (cancel-timer (diagram-viewer-mermaid--handle-timer handle)))
  (dolist (process (list (diagram-viewer-mermaid--handle-process handle)
                         (diagram-viewer-mermaid--handle-stderr-process handle)))
    (when (processp process)
      (ignore-errors (set-process-sentinel process #'ignore))
      (ignore-errors (delete-process process))))
  (dolist (path (list (diagram-viewer-mermaid--handle-input handle)
                      (diagram-viewer-mermaid--handle-config handle)))
    (when path (ignore-errors (delete-file path)))))

(defun diagram-viewer-mermaid--finish (handle status &optional error-data)
  "Clean HANDLE and deliver STATUS with ERROR-DATA exactly once."
  (unless (diagram-viewer-mermaid--handle-finished handle)
    (setf (diagram-viewer-mermaid--handle-finished handle) t)
    (when error-data
      (setq error-data
            (append error-data
                    (list :stdout (decode-coding-string
                                   (diagram-viewer-mermaid--handle-stdout handle) 'utf-8-unix)
                          :stderr (decode-coding-string
                                   (diagram-viewer-mermaid--handle-stderr handle) 'utf-8-unix)))))
    (diagram-viewer-mermaid--cleanup handle)
    (setf (diagram-viewer-mermaid--handle-stdout handle) nil
          (diagram-viewer-mermaid--handle-stderr handle) nil)
    (funcall (diagram-viewer-mermaid--handle-complete handle)
             (if error-data (list :status status :error error-data) (list :status status)))))

(defun diagram-viewer-mermaid--maybe-finish (handle)
  "Finish HANDLE after both main process and stderr pipe are terminal."
  (when (and (not (diagram-viewer-mermaid--handle-finished handle))
             (diagram-viewer-mermaid--handle-exit-status handle)
             (let ((pipe (diagram-viewer-mermaid--handle-stderr-process handle)))
               (or (null pipe) (memq (process-status pipe) '(closed exit signal failed)))))
    (let* ((exit (diagram-viewer-mermaid--handle-exit-status handle))
           (success (and (eq (car exit) 'exit) (= (cdr exit) 0))))
      (diagram-viewer-mermaid--finish
       handle (if success 'succeeded 'failed)
       (unless success
         (list :condition 'diagram-viewer-process-failed :message "mmdc did not exit successfully"
               :reason (car exit) :exit-code (cdr exit)))))))

(defun diagram-viewer-mermaid--stop (handle reason)
  "Stop HANDLE for REASON; the first terminal reason wins."
  (unless (or (diagram-viewer-mermaid--handle-finished handle)
              (diagram-viewer-mermaid--handle-reason handle))
    (setf (diagram-viewer-mermaid--handle-reason handle) reason)
    (diagram-viewer-mermaid--finish
     handle (if (eq reason 'cancelled) 'cancelled 'failed)
     (list :condition (if (eq reason 'cancelled) 'diagram-viewer-cancelled
                        'diagram-viewer-process-failed)
           :reason reason :message (if (eq reason 'cancelled) "Mermaid rendering cancelled"
                                   "Mermaid rendering timed out")))))

(defun diagram-viewer-mermaid-cancel (handle)
  "Cancel an opaque Mermaid HANDLE."
  (diagram-viewer-mermaid--stop handle 'cancelled))

(defun diagram-viewer-mermaid-render (request output complete)
  "Render immutable REQUEST into Core-owned OUTPUT, notifying COMPLETE.
The returned handle owns only process, timeout, logs and temporary inputs.
No current rendering customization is consulted here."
  (let* ((plan (diagram-viewer-request-execution-plan request))
         (context (diagram-viewer-request-context request))
         (handle (diagram-viewer-mermaid--handle-create
                  :complete complete :limit (plist-get plan :log-limit)
                  :stdout (unibyte-string) :stderr (unibyte-string))))
    (condition-case err
        (let ((coding-system-for-write 'utf-8-unix)
              (default-directory (file-name-as-directory (expand-file-name temporary-file-directory)))
              (process-environment (copy-sequence process-environment)))
          (when (file-remote-p default-directory)
            (signal 'diagram-viewer-invalid-argument '("Temporary directory must be local")))
          (when (plist-get plan :browser)
            (setenv "PUPPETEER_EXECUTABLE_PATH" (plist-get plan :browser)))
          (setf (diagram-viewer-mermaid--handle-input handle)
                (make-temp-file "diagram-viewer-mermaid-" nil ".mmd")
                (diagram-viewer-mermaid--handle-config handle)
                (make-temp-file "diagram-viewer-mermaid-" nil ".json"))
          (write-region (diagram-viewer-request-source request) nil
                        (diagram-viewer-mermaid--handle-input handle) nil 'silent)
          (write-region (plist-get context :config) nil
                        (diagram-viewer-mermaid--handle-config handle) nil 'silent)
          (setf (diagram-viewer-mermaid--handle-stderr-process handle)
                (make-pipe-process
                 :name "diagram-viewer-mermaid-stderr" :buffer nil :coding 'binary :noquery t
                 :filter (lambda (_ chunk) (diagram-viewer-mermaid--filter handle 'stderr chunk))
                 :sentinel (lambda (_ _event) (diagram-viewer-mermaid--maybe-finish handle))))
          (setf (diagram-viewer-mermaid--handle-process handle)
                (make-process
                 :name "diagram-viewer-mermaid" :buffer nil :coding 'binary :noquery t
                 :connection-type 'pipe
                 :stderr (diagram-viewer-mermaid--handle-stderr-process handle)
                 :command (list (plist-get plan :command)
                                "--input" (diagram-viewer-mermaid--handle-input handle)
                                "--output" output "--outputFormat" "svg"
                                "--theme" (plist-get context :theme)
                                "--backgroundColor" (plist-get context :background)
                                "--configFile" (diagram-viewer-mermaid--handle-config handle))
                 :filter (lambda (_ chunk) (diagram-viewer-mermaid--filter handle 'stdout chunk))
                 :sentinel
                 (lambda (process _event)
                   (when (memq (process-status process) '(exit signal failed))
                     (setf (diagram-viewer-mermaid--handle-exit-status handle)
                           (cons (process-status process) (process-exit-status process)))
                     (diagram-viewer-mermaid--maybe-finish handle)))))
          (unless (diagram-viewer-mermaid--handle-finished handle)
            (setf (diagram-viewer-mermaid--handle-timer handle)
                  (run-at-time (plist-get plan :timeout) nil
                               #'diagram-viewer-mermaid--stop handle 'timeout))))
      ((error quit)
       (diagram-viewer-mermaid--finish
        handle 'failed (list :condition 'diagram-viewer-process-failed
                             :reason 'startup :message (error-message-string err)))))
    handle))

(defun diagram-viewer-mermaid-enable (&optional replace)
  "Register the Mermaid renderer, optionally allowing REPLACE.
Registration does not require mmdc or a browser to be installed."
  (interactive "P")
  (diagram-viewer-register-renderer
   (diagram-viewer-renderer-create
    :id 'mermaid :languages '(mermaid) :output-extension ".svg"
    :available-p #'diagram-viewer-mermaid-available-p
    :cache-context-function #'diagram-viewer-mermaid-cache-context
    :render-function #'diagram-viewer-mermaid-render
    :cancel-function #'diagram-viewer-mermaid-cancel)
   replace))

(provide 'diagram-viewer-adapter-mermaid)
;;; diagram-viewer-adapter-mermaid.el ends here
