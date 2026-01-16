;;; core-impl.el --- Stargate Main Orchestrator implementation -*- lexical-binding: t -*-

(require 'dragonruby-kernel)
(require 'dragonruby-stargate-manager)
(require 'dragonruby-stargate-tracker)
(require 'dragonruby-stargate-bridge)
(require 'dragonruby-stargate-timeline)
(require 'dragonruby-stargate-injector)

(defvar-local dragonruby-stargate--mode-line-indicator nil
  "Mode-line indicator for Stargate status.")

(put 'dragonruby-stargate--mode-line-indicator 'risky-local-variable t)

;;;###autoload
(defun dragonruby-stargate-enable ()
  "Initialize and enable the Stargate module as a Kernel Organ."
  (interactive)
  (let ((root (dragonruby--find-project-root)))
    (if root
        (progn
          (dragonruby-stargate-session-init root)
          
          ;; Wire the Chronicler: Attach to the Kernel Pulse
          (dragonruby-kernel-register-hook 'dragonruby-monitor-hook 
                                         #'dragonruby-stargate--heartbeat t)
          
          ;; Wire the Surgeon: Atomic Injections on Save
          (dragonruby-kernel-register-hook 'after-save-hook 
                                         #'dragonruby-stargate-inject-buffer t)
          
          (setq dragonruby-stargate--mode-line-indicator
                (propertize " 🌌" 'face 'success 'help-echo "Stargate Chronicler Cabled"))
          (add-to-list 'mode-line-process '(:eval dragonruby-stargate--mode-line-indicator))
          
          (message "🚀 STARGATE: Active in root [%s]" root))
      (error "STARGATE: Unable to find DragonRuby project root"))))

;;;###autoload
(defun dragonruby-stargate-disable ()
  "Deactivate and cleanup the Stargate module."
  (interactive)
  (remove-hook 'dragonruby-monitor-hook #'dragonruby-stargate--heartbeat t)
  (remove-hook 'after-save-hook #'dragonruby-stargate-inject-buffer t)
  (setq dragonruby-stargate--mode-line-indicator nil)
  (message "💤 STARGATE: Organ silenced."))

(defun dragonruby-stargate--heartbeat ()
  "The organic pulse of Stargate. Checked by the Kernel Heartbeat."
  (let ((connected (and (processp dragonruby-stargate-bridge--process)
                        (process-live-p dragonruby-stargate-bridge--process))))
    (unless connected
      (dragonruby-stargate-bridge-find-and-install t))
    
    ;; Update indicator aesthetics
    (setq dragonruby-stargate--mode-line-indicator
          (if connected
              (propertize " 🌌" 'face 'success 'help-echo "Stargate Chronicler Cabled")
            (propertize " 💤" 'face 'shadow 'help-echo "Stargate: Searching for Dragon...")))))

;;;###autoload
(defun dragonruby-stargate-inject-buffer (&optional file)
  "Inject the current buffer into the simulation with risk classification."
  (interactive)
  (let* ((code (buffer-substring-no-properties (point-min) (point-max))))
    (dragonruby-stargate-injector-transmit code)
    (dragonruby-stargate-tracker-update-hash (or file (buffer-file-name)))))

(provide 'dragonruby-stargate-core--impl)
;;; core-impl.el ends here
