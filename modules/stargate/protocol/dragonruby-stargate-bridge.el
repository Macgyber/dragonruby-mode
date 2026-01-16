;;; dragonruby-stargate-bridge.el --- Stargate Bridge for DragonRuby -*- lexical-binding: t -*-

(require 'json)
(require 'dragonruby-utils)

(defvar dragonruby-stargate-bridge--process nil)
(defvar dragonruby-stargate-bridge--partial-buffer "")

(defvar-local dragonruby-stargate-bridge-event-hook nil
  "Hook run when a Stargate event is received from the runtime.")

(defun dragonruby-stargate-bridge--strip-ansi (string)
  "Remove ANSI escape codes from STRING."
  (replace-regexp-in-string "\e\\[[0-9;]*[a-zA-Z]" "" string))

(defun dragonruby-stargate-bridge--filter (process output)
  "Filter and capture [STARGATE_MOMENT] events from PROCESS OUTPUT."
  ;; 1. Accumulate raw output (Fragmentation handling)
  (setq dragonruby-stargate-bridge--partial-buffer 
        (concat dragonruby-stargate-bridge--partial-buffer output))
  
  ;; 2. Process complete lines only
  (while (string-match "\\(\\[STARGATE_MOMENT\\].*\\)\\(?:\n\\|\r\n\\)" 
                       dragonruby-stargate-bridge--partial-buffer)
    (let* ((line (match-string 1 dragonruby-stargate-bridge--partial-buffer))
           (clean-line (dragonruby-stargate-bridge--strip-ansi line))
           (end-pos (match-end 0)))
      
      (if (string-match "{.*}" clean-line)
          (let ((json-packet (match-string 0 clean-line)))
            (message "📡 Stargate Bridge: Found moment packet (%d bytes)" (length json-packet))
            (dragonruby-stargate-bridge-handle-event json-packet))
        (message "⚠️ Stargate Bridge: No JSON block found in [STARGATE_MOMENT] line."))
      
      ;; 4. Consume buffer
      (setq dragonruby-stargate-bridge--partial-buffer 
            (substring dragonruby-stargate-bridge--partial-buffer end-pos))))
  
  ;; Safety cleanup if buffer gets too large without a match
  (when (> (length dragonruby-stargate-bridge--partial-buffer) 500000)
    (message "⚠️ Stargate Bridge: Buffer overflow, clearing.")
    (setq dragonruby-stargate-bridge--partial-buffer "")))

(defun dragonruby-stargate-bridge-handle-event (json-string)
  "Parse and distribute a JSON-STRING event from the runtime."
  (condition-case err
      (let* ((json-object-type 'alist)
             (json-key-type 'string)
             (event (json-read-from-string json-string)))
        
        ;; Protocol Contract Validation (The Book of Laws)
        (let ((type (cdr (assoc "type" event)))
              (obs (cdr (assoc "observed_at" event))))
          (cond
           ((not type) 
            (message "🛑 Stargate Bridge: Malformed packet ignored (Missing 'type')."))
           ((and (string= type "moment") (not (assoc "moment_type" event)))
            (message "⚠️ Stargate Bridge: DEPRECATED PACKET - Missing 'moment_type'."))
           ((and (string= type "moment") (not (listp obs)))
            (message "🛑 Stargate Bridge: CONTRACT VIOLATION - 'observed_at' must be a struct."))
           (t
            (run-hook-with-args 'dragonruby-stargate-bridge-event-hook event)))))
    (error
     (message "❌ Stargate Bridge: JSON Parse Error: %s" err)
     ;; Log a snippet of the failed string for debugging
     (message "🔗 Segment: %s..." (substring json-string 0 (min (length json-string) 100))))))

(defun dragonruby-stargate-bridge-send-code (code)
  "Send CODE to the DragonRuby runtime via the bridge."
  (if (and dragonruby-stargate-bridge--process (process-live-p dragonruby-stargate-bridge--process))
      (process-send-string dragonruby-stargate-bridge--process (concat code "\n"))
    (message "❌ Stargate Bridge: Connection lost.")))

(defun dragonruby-stargate-bridge-find-and-install (&optional silent)
  "Find the DragonRuby simulation process and install the filter.
If SILENT is non-nil, don't message when the process is not found."
  (interactive)
  ;; DragonRuby Toolkit uses "dragonruby" as the process name.
  (let ((proc (get-process "dragonruby")))
    (if proc
        (progn
          (setq dragonruby-stargate-bridge--process proc)
          (set-process-filter proc #'dragonruby-stargate-bridge--filter)
          (message "📡 Stargate Bridge: Cabled to DragonRuby Simulation."))
      (unless silent
        (message "❌ Stargate Bridge: Could not find 'dragonruby' process.")))))

(defalias 'dragonruby-stargate-bridge-install #'dragonruby-stargate-bridge-find-and-install)
(defalias 'stargate-bridge-cable #'dragonruby-stargate-bridge-find-and-install)

(defun dragonruby-stargate-bridge-diagnostic ()
  "Run a diagnostic on the Stargate communication bridge."
  (interactive)
  (message "🔍 Stargate Diagnostic: ---------------------------")
  (message "🔍 Bridge Process: %s" (if dragonruby-stargate-bridge--process (process-name dragonruby-stargate-bridge--process) "NONE"))
  (message "🔍 Process Alive: %s" (if (and dragonruby-stargate-bridge--process (process-live-p dragonruby-stargate-bridge--process)) "YES" "NO"))
  (message "🔍 Buffer Length: %d" (length dragonruby-stargate-bridge--partial-buffer))
  (if (and dragonruby-stargate-bridge--process (process-live-p dragonruby-stargate-bridge--process))
      (dragonruby-stargate-bridge-send-code "puts '[STARGATE_DEBUG] PONG: Communication Bridge is CABLED.'")
    (dragonruby-stargate-bridge-find-and-install))
  (message "🔍 Stargate Diagnostic: ---------------------------"))

(provide 'dragonruby-stargate-bridge)
;;; bridge.el ends here
