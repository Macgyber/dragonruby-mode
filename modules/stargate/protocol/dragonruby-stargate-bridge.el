;;; dragonruby-stargate-bridge.el --- Stargate Bridge for DragonRuby -*- lexical-binding: t -*-

;; Author: Macgyber <esteban3261g@gmail.com>
;; Version: 0.8.0
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/Macgyber/dragonruby-mode

;;; Commentary:
;; Communication layer that captures [STARGATE_MOMENT] markers from the
;; DragonRuby process output and distributes them as JSON events.

;;; Code:

(require 'json)
(require 'dragonruby-utils)

(defvar dragonruby-stargate-bridge--process nil)
(defvar dragonruby-stargate-bridge--debug t "Enable atomic debugging for Stargate communication.")

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
  "Filter incoming OUTPUT from PROCESS using optimized batch processing."
  (when (and dragonruby-stargate-bridge--debug (not (string-blank-p output)))
    (message "📡 [BRIDGE <<] %s" (string-trim output)))
  (let ((work-buf (dragonruby-stargate-bridge--get-buffer)))
    (when (buffer-live-p work-buf)
      (with-current-buffer work-buf
        ;; 1. Void Shield: Ignore massive render storms
        (when (> (length output) 10000)
          (when (string-match-p "Render\\|Engine" output)
            (unless (string-match-p "moment\\|infection" output)
              (setq output "[STARGATE] Void Shield: Noise Filtered.\n"))))

        (save-excursion
          (goto-char (point-max))
          (insert output))
        
        ;; 2. Line Processing
        (goto-char (point-min))
        (let ((limit (save-excursion (goto-char (point-max)) (line-beginning-position))))
          (while (< (point) limit)
            (let* ((line-end (line-end-position))
                   (line (buffer-substring-no-properties (line-beginning-position) line-end)))
              ;; Scrub ANSI
              (setq line (replace-regexp-in-string "\e\\[[0-9;]*[mKk]" "" line))
              
              (when (string-match-p "RNG seed has been set to" line)
                (message "🚀 STARGATE: DragonRuby VM is STABLE. Ready for leap."))
              
              (when (string-match-p "\\(stargate\\|moment\\|infection\\|SUCCESS\\|{\\)" line)
                (dragonruby-stargate-bridge--handle-event line))
              
              (forward-line 1)))
          
          ;; 3. Cleanup
          (delete-region (point-min) (point)))))))

(defun dragonruby-stargate-bridge--handle-event (payload)
  "Parse and distribute a PAYLOAD (JSON or ACK) from the runtime."
  (condition-case nil
      ;; Strategy: Search for the JSON block within the payload if present
      (let ((json-start (string-match "{" payload)))
        (if json-start
            (let* ((json-string (substring payload json-start))
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
           ((string-match "STARGATE: Interposition SUCCESSful" payload)
            (run-hook-with-args 'dragonruby-stargate-bridge-event-hook '((type . "infection_ack"))))
           ((string-match "🛑 STARGATE: Simulation PAUSED" payload)
            (run-hook-with-args 'dragonruby-stargate-bridge-event-hook '((type . "stasis_ack"))))
           ((string-match "▶️ STARGATE: Simulation RESUMED" payload)
            (run-hook-with-args 'dragonruby-stargate-bridge-event-hook '((type . "record_ack")))))))
    (error nil)))

(defun dragonruby-stargate-bridge-send-code (code)
  "Send CODE to the DragonRuby runtime via the bridge."
  (when dragonruby-stargate-bridge--debug
    (message "📡 [BRIDGE >>] %s" code))
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
;;; dragonruby-stargate-bridge.el ends here
