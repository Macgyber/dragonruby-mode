;;; dragonruby-stargate-fsm.el --- Stargate Finite State Machine -*- lexical-binding: t -*-

(defvar dragonruby-stargate--state nil "Current high-level state of the Stargate system.")

(defun dragonruby-stargate--set-state (new-state)
  "Transition Stargate to NEW-STATE with logging and safety checks."
  (let ((connected (and (boundp 'dragonruby-stargate-bridge--process)
                        dragonruby-stargate-bridge--process
                        (process-live-p dragonruby-stargate-bridge--process))))
    
    ;; Safety: Cannot be ACTIVE without a bridge, but initializing is okay.
    (when (and (eq new-state :active) (not connected))
      (setq new-state nil)
      (message "🌌 STARGATE: [ERROR] Cannot set ACTIVE without live bridge."))

    (unless (eq dragonruby-stargate--state new-state)
      (let ((old dragonruby-stargate--state))
        (setq dragonruby-stargate--state new-state)
        (message "🌌 STARGATE: %s ➔ %s" (or old "IDLE") (or new-state "IDLE"))))))

(provide 'dragonruby-stargate-fsm)
