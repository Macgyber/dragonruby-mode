;;; dragonruby-stargate-bridge.el --- Stargate Bridge for DragonRuby -*- lexical-binding: t -*-

(require 'json)
(require 'dragonruby-utils)

(defvar dragonruby-stargate-bridge--process nil)

(defvar-local dragonruby-stargate-bridge-event-hook nil
  "Hook run when a Stargate event is received from the runtime.")

(defun dragonruby-stargate-bridge--strip-ansi (string)
  "Remove ANSI escape codes from STRING."
  (replace-regexp-in-string "\e\\[[0-9;]*[a-zA-Z]" "" string))

(defvar dragonruby-stargate-bridge--buffer-name " *stargate-bridge-work*")

(defun dragonruby-stargate-bridge--get-buffer ()
  "Get or create the hidden storage buffer for bridge telemetry."
  (get-buffer-create dragonruby-stargate-bridge--buffer-name))

(defun dragonruby-stargate-bridge--filter (process output)
  "Filter and capture [STARGATE_MOMENT] events from PROCESS OUTPUT using buffer-based scanning."
  (with-current-buffer (dragonruby-stargate-bridge--get-buffer)
    ;; 1. Append new output to working buffer
    (goto-char (point-max))
    (insert output)
    
    ;; 2. Scan for complete [STARGATE_MOMENT] lines
    (goto-char (point-min))
    (let ((marker "[STARGATE_MOMENT] "))
      (while (search-forward marker nil t)
        (let ((start (point))
              (end (line-end-position)))
          (if (< end (point-max)) ;; Complete line found
              (let ((line (buffer-substring-no-properties start end)))
                (let ((json-start (string-match "{" line)))
                  (when json-start
                    (dragonruby-stargate-bridge-handle-event (substring line json-start))))
                (delete-region (line-beginning-position) (1+ end))
                (goto-char (point-min)))
            (goto-char (point-max)))))) ;; Incomplete line, exit loop
    
    ;; 3. Safety: Trim ancient history if buffer grows too large
    (when (> (buffer-size) 100000)
      (goto-char (point-min))
      ;; Keep only the last 50k if it's flooded without markers
      (delete-region (point-min) (- (point-max) 50000)))))

(defun dragonruby-stargate-bridge-handle-event (json-string)
  "Parse and distribute a JSON-STRING event from the runtime."
  (condition-case err
      (let* ((json-object-type 'alist)
             (json-key-type 'string)
             (event (json-read-from-string json-string)))
        
        ;; Protocol Contract Validation (The Book of Laws)
        ;; We use a more robust extraction that handles both string and symbol keys
        (let* ((get-val (lambda (key) 
                          (cdr (or (assoc (if (symbolp key) (symbol-name key) key) event)
                                   (assoc (if (stringp key) (intern key) key) event)))))
               (type (funcall get-val "type"))
               (obs  (funcall get-val "observed_at"))
               (mtype (funcall get-val "moment_type")))
          (cond
           ((not type) 
            (message "🛑 Stargate Bridge: Malformed packet ignored (Missing 'type'). JSON: %s" json-string))
           ((and (string= type "moment") (not mtype))
            (message "⚠️ Stargate Bridge: DEPRECATED PACKET - Missing 'moment_type'."))
            ((and (string= type "moment") (not (numberp obs)))
             (message "🛑 Stargate Bridge: CONTRACT VIOLATION - 'observed_at' must be an integer (is %S). JSON: %s" 
                      obs json-string))
            ((string= type "divergence")
             (let ((addr (funcall get-val "address"))
                   (exp (funcall get-val "expected"))
                   (act (funcall get-val "actual")))
               (message "⚡ STARGATE: DIVERGENCE DETECTED at %s!" addr)
               (message "   Expected Hash: %s" exp)
               (message "   Actual Hash:   %s" act)
               (run-hook-with-args 'dragonruby-stargate-bridge-event-hook event)))
            (t
             (run-hook-with-args 'dragonruby-stargate-bridge-event-hook event)))))
    (error
     (message "❌ Stargate Bridge: JSON Parse Error: %s" err)
     (message "🔗 Raw Packet: %s" json-string))))

(defun dragonruby-stargate-bridge-send-code (code)
  "Send CODE to the DragonRuby runtime via the bridge."
  (if (and dragonruby-stargate-bridge--process (process-live-p dragonruby-stargate-bridge--process))
      (process-send-string dragonruby-stargate-bridge--process (concat code "\n"))
    (message "❌ Stargate Bridge: Connection lost.")))

(defun dragonruby-stargate-bridge-find-and-install (&optional silent)
  "Find the DragonRuby simulation process and install the filter.
If SILENT is non-nil, don't message when the process is not found."
  (interactive)
  ;; DragonRuby Toolkit uses "dragonruby" as the process name via (dragonruby-run).
  (let ((proc (get-process "dragonruby")))
    (if (and proc (process-live-p proc))
        (progn
          (setq dragonruby-stargate-bridge--process proc)
          ;; Only set filter if not already set to our filter
          (unless (eq (process-filter proc) #'dragonruby-stargate-bridge--filter)
            (set-process-filter proc #'dragonruby-stargate-bridge--filter))
          (unless silent
            (message "📡 Stargate Bridge: Cabled to DragonRuby Simulation."))
          t) ;; Return t on success
      (progn
        (unless silent
          (message "❌ Stargate Bridge: No active 'dragonruby' process found. Launch the game with M-x dragonruby-run first."))
        nil))))

(defalias 'dragonruby-stargate-bridge-install #'dragonruby-stargate-bridge-find-and-install)
(defalias 'stargate-bridge-cable #'dragonruby-stargate-bridge-find-and-install)

(defun dragonruby-stargate-bridge-diagnostic ()
  "Run a diagnostic on the Stargate communication bridge."
  (interactive)
  (message "🔍 Stargate Diagnostic: ---------------------------")
  (message "🔍 Bridge Process: %s" (if dragonruby-stargate-bridge--process (process-name dragonruby-stargate-bridge--process) "NONE"))
  (message "🔍 Process Alive: %s" (if (and dragonruby-stargate-bridge--process (process-live-p dragonruby-stargate-bridge--process)) "YES" "NO"))
  (message "🔍 Buffer Length: %d" (buffer-size (dragonruby-stargate-bridge--get-buffer)))
  (if (and dragonruby-stargate-bridge--process (process-live-p dragonruby-stargate-bridge--process))
      (dragonruby-stargate-bridge-send-code "puts '[STARGATE_DEBUG] PONG: Communication Bridge is CABLED.'")
    (dragonruby-stargate-bridge-find-and-install))
  (message "🔍 Stargate Diagnostic: ---------------------------"))

(provide 'dragonruby-stargate-bridge)
;;; bridge.el ends here
