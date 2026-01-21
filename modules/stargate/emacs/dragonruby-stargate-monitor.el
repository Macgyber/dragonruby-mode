;;; dragonruby-stargate-monitor.el --- Stargate Process & Health Monitor -*- lexical-binding: t -*-

(require 'dragonruby-stargate-bridge)

(defun dragonruby-stargate-monitor-pulse ()
  "Heartbeat of the Stargate monitoring system."
  (let ((connected (and dragonruby-stargate-bridge--process 
                        (process-live-p dragonruby-stargate-bridge--process))))
    
    ;; 1. Automatic reconnection / initialization attempt
    (unless connected
      (when (dragonruby-stargate-bridge-find-and-install nil t)
        (setq connected t)))

    ;; 2. Lifecycle orchestration
    (if connected
        (when (and (eq dragonruby-stargate--state :initializing)
                   dragonruby-stargate--runtime-ready)
          (dragonruby-stargate--set-state :active))
      (progn
        (when (eq dragonruby-stargate--state :active)
          (dragonruby-stargate-session-stop))))

    ;; 3. UI Update
    (dragonruby-stargate-status-refresh)))

;; Integrate with the central scheduler
(add-hook 'dragonruby-monitor-hook #'dragonruby-stargate-monitor-pulse)

(provide 'dragonruby-stargate-monitor)
