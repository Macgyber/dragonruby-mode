;;; dragonruby-stargate-status-view.el --- Stargate Status Renderer (Classic Edition) -*- lexical-binding: t -*-

(require 'dragonruby-stargate-sessions)
(require 'dragonruby-stargate-telemetry)
(require 'dragonruby-stargate-bridge)
(require 'dragonruby-stargate-fsm)

(defun dragonruby-stargate-status-view--render ()
  "Render the classic Stargate Status dashboard with guaranteed visibility."
  (let* ((proc (and (boundp 'dragonruby-stargate-bridge--process)
                    dragonruby-stargate-bridge--process))
         (connected (and proc (process-live-p proc)))
         (ready (and (boundp 'dragonruby-stargate--runtime-ready) dragonruby-stargate--runtime-ready))
         (index (and (boundp 'dragonruby-stargate--session-index) dragonruby-stargate--session-index))
         (state (and (boundp 'dragonruby-stargate--state) dragonruby-stargate--state))
         (session-id (if index (cdr (assoc "session_id" index)) "NONE"))
         (frames (if index (hash-table-count (cdr (assoc "moments" index))) 0))
         (engine-id (if connected (process-id proc) "---")))

    ;; Header
    (insert (propertize "🐉 DragonRuby — Stargate Status\n" 'face '(:weight bold :height 1.4)))
    (insert (propertize "────────────────────────────────────────────────────────────\n" 'face 'shadow))
    (insert "\n")

    ;; Stargate Systems Section
    (insert (propertize "Stargate Systems\n" 'face '(:weight bold :height 1.1)))
    
    (insert "  Status     : " 
            (if connected 
                (propertize "💤 Connected & Cabled" 'face 'shadow)
              (propertize "❌ Disconnected" 'face 'error)) "\n")
              
    (insert "  Engine ID  : " (propertize (format "%s" engine-id) 'face 'font-lock-variable-name-face) "\n")
    
    (insert "  Monitor    : " 
            (if ready 
                (propertize "✔  Global Sweep Active" 'face 'success)
              (propertize "⌛ Waiting for Runtime..." 'face 'warning)) "\n")
              
    (insert "  Session    : " (propertize session-id 'face 'font-lock-string-face) "\n")
    (insert "  Frames     : " (propertize (number-to-string frames) 'face 'font-lock-constant-face) "\n")
    (insert "\n")

    ;; Operational Controls
    (insert (propertize "Operational Controls\n" 'face '(:weight bold :height 1.1)))
    (insert "  • " (propertize "i" 'face 'font-lock-keyword-face) " : Manual Infection (Force Cable)\n")
    (insert "  • " (propertize "F7" 'face 'font-lock-keyword-face) ": Start/Resume Recording\n")
    (insert "  • " (propertize "F8" 'face 'font-lock-keyword-face) ": Pause (Stasis)\n")
    (insert "  • " (propertize "F9" 'face 'font-lock-keyword-face) ": Open Temporal Timeline\n")
    (insert "  • " (propertize "g" 'face 'font-lock-keyword-face)  " : Refresh Radar\n")
    (insert "  • " (propertize "q" 'face 'font-lock-keyword-face)  " : Close Console\n")
    (insert "\n")
    
    (insert (propertize "────────────────────────────────────────────────────────────\n" 'face 'shadow))))

(provide 'dragonruby-stargate-status-view)
