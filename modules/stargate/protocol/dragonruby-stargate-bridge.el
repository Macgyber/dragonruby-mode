;;; dragonruby-stargate-bridge.el --- Stargate Bridge for DragonRuby -*- lexical-binding: t -*-

;; Author: Macgyber <esteban3261g@gmail.com>
;; Version: 0.8.0
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/Macgyber/dragonruby-mode

;;; Commentary:
;; Communication layer that captures [STARGATE_MOMENT] markers from the
;; DragonRuby process output and distributes them as JSON events.

;;; Code:

(message "📡 STARGATE BRIDGE: [NERVE-TRACKER] Version 1.1.2 Loaded (Absolute Hardened).")

(require 'json)
(require 'dragonruby-utils)

(defvar dragonruby-stargate-bridge--process nil)
(defvar dragonruby-stargate-bridge--debug t "Enable atomic debugging for Stargate communication.")
(defvar dragonruby-stargate-bridge--processing nil "Guard against re-entrant processing.")

(defvar dragonruby-stargate-bridge-event-hook nil
  "Hook run when a Stargate event is received from the runtime.")

(defun dragonruby-stargate-bridge--strip-ansi (string)
  "Remove ANSI escape codes from STRING."
  (replace-regexp-in-string "\e\\[[0-9;]*[a-zA-Z]" "" string))

(defvar dragonruby-stargate-bridge--buffer-name " *stargate-bridge-work*")

(defun dragonruby-stargate-bridge--get-buffer ()
  "Get or create the hidden storage buffer for bridge telemetry."
  (get-buffer-create dragonruby-stargate-bridge--buffer-name))

(defun dragonruby-stargate-bridge--filter (process output)
  "Minimalist output aggregator for Stargate PROTCOL (Step 1).
Failsafe: Mirrors output back to the Simulation buffer for visibility."
  (let ((work-buf (dragonruby-stargate-bridge--get-buffer))
        (sim-buf (process-buffer process)))
    ;; 1. Visibility Mirror (So the Architect isn't blind)
    (when (buffer-live-p sim-buf)
      (with-current-buffer sim-buf
        (save-excursion
          (goto-char (point-max))
          (let ((inhibit-read-only t))
            (insert output)))))

    ;; 2. Deep Logging (Loud and clear)
    (message "📡 BRIDGE: Filter triggered (%d bytes from %s)" 
             (length output) (process-name process))

    ;; 3. State Accumulation
    (when (buffer-live-p work-buf)
      (with-current-buffer work-buf
        (save-excursion
          (goto-char (point-max))
          (insert output)))
      ;; Trigger processing (Step 2)
      (dragonruby-stargate-bridge--process-buffer))))

(defun dragonruby-stargate-bridge--process-buffer ()
  "Process complete lines in the bridge buffer (Step 2).
Sovereign Invariants: Re-entrant safe, Idempotent, Line-Safe."
  (let ((work-buf (dragonruby-stargate-bridge--get-buffer)))
    (when (and (buffer-live-p work-buf)
               (not dragonruby-stargate-bridge--processing))
      (setq dragonruby-stargate-bridge--processing t)
      (unwind-protect
          (with-current-buffer work-buf
            (goto-char (point-min))
            (let ((last-newline (save-excursion
                                  (goto-char (point-max))
                                  (when (search-backward "\n" nil t)
                                    (point)))))
              (if (not last-newline)
                  (when (and dragonruby-stargate-bridge--debug (> (buffer-size) 0))
                    (message "📡 BRIDGE: Buffer has %d bytes but NO newline yet. Waiting..." (buffer-size)))
                ;; Process found lines
                (while (< (point) last-newline)
                  (let* ((line-end (line-end-position))
                         (line (buffer-substring-no-properties (line-beginning-position) line-end)))
                    
                    ;; 1. Global Debug Logging (Before Scrub)
                    (when (and dragonruby-stargate-bridge--debug (not (string-blank-p line)))
                      (message "📡 [BRIDGE <<] %s" (string-trim line)))

                    ;; 2. ANSI Scrubbing (Step 3)
                    (setq line (replace-regexp-in-string "\e\\[[0-9;]*[mKk]" "" line))
                    
                    ;; 3. State Stability Markers
                    (when (string-match-p "RNG seed has been set to" line)
                      (message "🚀 STARGATE: DragonRuby VM is STABLE. Ready for leap."))

                    ;; 4. Event Detection (Step 4)
                    ;; Sovereign Law: Only process lines that are explicitly STARGATE signaled or JSON objects.
                    (when (string-match-p "^\\(STARGATE\\|\\[STARGATE\\|\\[moment\\|{\\)" line)
                      (dragonruby-stargate-bridge--handle-event line))
                    
                    (forward-line 1)))
                
                ;; 5. Surgical Cleanup: Only what we processed
                (delete-region (point-min) last-newline))))
        (setq dragonruby-stargate-bridge--processing nil)))))

(defun dragonruby-stargate-bridge--handle-event (payload)
  "Parse and distribute a PAYLOAD (JSON or ACK) from the runtime.
Invariants: Never mutates the bridge buffer, provides debug visibility."
  (condition-case err
      ;; Strategy: Search for the JSON block within the payload if present
      ;; HARDENED: Only treat as JSON if it contains a '"type"' key.
      (let ((json-start (string-match "{\\(?:.*\\)\"type\"" payload)))
        (if json-start
            (let* ((json-string (substring payload (string-match "{" payload)))
                   (json-object-type 'alist)
                   (json-key-type 'string)
                   (event (json-read-from-string json-string))
                   (get-val (lambda (key) 
                              (cdr (or (assoc (if (symbolp key) (symbol-name key) key) event)
                                       (assoc (if (stringp key) (intern key) key) event)))))
                   (type (funcall get-val "type")))
              
              (cond
               ((string= type "divergence")
                (message "⚡ STARGATE: DIVERGENCE DETECTED!")
                (run-hook-with-args 'dragonruby-stargate-bridge-event-hook event))
               ((string= type "moment")
                (run-hook-with-args 'dragonruby-stargate-bridge-event-hook event))
               ((string= type "pong")
                (message "📡 STARGATE: Pong received! Heartbeat confirmed."))
               (t
                (run-hook-with-args 'dragonruby-stargate-bridge-event-hook event))))
          
          ;; Fallback: Non-JSON confirmation signals (ACKs)
          (cond
           ((string-match "STARGATE: \\[\\(TRACE\\|DEBUG\\|ERROR\\)\\]" payload)
            (message "📡 [RUBY] %s" (string-trim payload)))
           ((string-match "STARGATE::INFECTED" payload)
            (run-hook-with-args 'dragonruby-stargate-bridge-event-hook '((type . "infection_ack"))))
           ((string-match "🛑 STARGATE: Simulation PAUSED" payload)
            (run-hook-with-args 'dragonruby-stargate-bridge-event-hook '((type . "stasis_ack"))))
           ((string-match "▶️ STARGATE: Simulation RESUMED" payload)
            (run-hook-with-args 'dragonruby-stargate-bridge-event-hook '((type . "record_ack")))))))
    (error
     (when dragonruby-stargate-bridge--debug
       (message "📡 [BRIDGE] JSON/Event Error: %S in payload: %S" err payload)))))

(defun dragonruby-stargate-bridge-send-code (code)
  "Send CODE to the DragonRuby runtime via the bridge.

⚠️  DEPRECATED (Law XIX: Hot Reload Contract) ⚠️

This function attempts to send commands via STDIN, but DragonRuby is NOT a REPL
and does not read commands from STDIN. This will be silently ignored by DragonRuby.

Use file-based injection instead:
  - Write code to a .rb file in mygame/
  - DragonRuby's hot-reload will detect and load it automatically

This function is kept for diagnostic purposes only."
  (declare (obsolete "Use file-based injection instead" "0.8.1"))
  (warn "dragonruby-stargate-bridge-send-code is deprecated. DragonRuby doesn't read STDIN. Use file-based injection (Law XIX).")
  (when dragonruby-stargate-bridge--debug
    (message "📡 [BRIDGE >>] (DEPRECATED - WILL BE IGNORED) %s" code))
  (if (and dragonruby-stargate-bridge--process (process-live-p dragonruby-stargate-bridge--process))
      (let ((coding-system-for-write 'utf-8-dos))
        (process-send-string dragonruby-stargate-bridge--process (concat code "\r\n")))
    (message "❌ Stargate Bridge: Connection lost.")))

(defun dragonruby-stargate-bridge-find-and-install (&optional process silent)
  "Find the DragonRuby simulation process and install the filter.
If PROCESS is provided, use it. If SILENT is non-nil, suppress messages."
  (interactive)
  (let ((proc (or process (get-process "dragonruby"))))
    (if (and proc (process-live-p proc))
        (progn
          (setq dragonruby-stargate-bridge--process proc)
          (set-process-filter proc #'dragonruby-stargate-bridge--filter)
          (set-process-coding-system proc 'utf-8-dos 'utf-8-dos)
          (dragonruby-stargate-bridge-toggle-debug dragonruby-stargate-bridge--debug)
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
  "Run a deep diagnostic and ATTEMPT RECOVERY of the bridge."
  (interactive)
  ;; 1. Emergency Reset (Unlock the Bridge)
  (setq dragonruby-stargate-bridge--processing nil)
  
  (let* ((proc (get-process "dragonruby"))
         (filter (and proc (process-filter proc)))
         (buf (dragonruby-stargate-bridge--get-buffer))
         (source (symbol-file 'dragonruby-stargate-bridge-diagnostic)))
    (message "🔍 STARGATE: [NERVE-TRACKER] --------------------")
    (message "🔍 Version: 1.1.2 (Hardened)")
    (message "🔍 Source : %s" source)
    (message "🔍 Lock   : %s (Resetting to nil now...)" dragonruby-stargate-bridge--processing)
    (message "🔍 Process Found: %s (%s)" (if proc "YES" "NO") (if proc (process-name proc) "-"))
    (message "🔍 Process Alive: %s" (if (and proc (process-live-p proc)) "YES" "NO"))
    (message "🔍 Process Filter: %s" filter)
    (message "🔍 Bridge Buffer: %s (Size: %d)" (buffer-name buf) (buffer-size buf))
    
    (when (and proc (process-live-p proc))
      ;; Force re-attachment if missing
      (unless (eq filter #'dragonruby-stargate-bridge--filter)
        (message "📡 BRIDGE: Re-attaching filter (Safety Catch)...")
        (set-process-filter proc #'dragonruby-stargate-bridge--filter))
      
      (message "📡 Sending Canonical Probes to DragonRuby...")
      (dragonruby-stargate-bridge-send-code "puts '[STARGATE_DEBUG] PROBE: Authority=' + (Object.respond_to?(:tick, true) && Object.new.method(:tick).owner.to_s.include?('Interposition') ? 'YES' : 'NO')")
      (dragonruby-stargate-bridge-send-code "puts '[STARGATE_DEBUG] PONG: Communication Bridge is CABLED.'"))
    (message "🔍 STARGATE: [NERVE-TRACKER] --------------------")))

(defun dragonruby-stargate-bridge-toggle-debug (&optional state)
  "Toggle atomic debugging for the bridge and sync with runtime.
If STATE is provided (t or nil), set it explicitly."
  (interactive)
  (setq dragonruby-stargate-bridge--debug (if (null state) (not dragonruby-stargate-bridge--debug) state))
  (let ((val (if dragonruby-stargate-bridge--debug "true" "false")))
    (dragonruby-stargate-bridge-send-code (format "$stargate_debug = %s" val))
    (message "📡 STARGATE: Atomic Debug is now %s." (if dragonruby-stargate-bridge--debug "ON" "OFF")))
  ;; Refresh dashboard if fbound
  (when (fboundp 'dragonruby-stargate-status-refresh)
    (dragonruby-stargate-status-refresh)))

(provide 'dragonruby-stargate-bridge)
;;; dragonruby-stargate-bridge.el ends here
