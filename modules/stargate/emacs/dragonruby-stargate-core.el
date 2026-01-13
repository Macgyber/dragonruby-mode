;;; core.el --- Stargate Main Orchestrator -*- lexical-binding: t -*-

;;; Commentary:
;; This is the main orchestrator for the Stargate module.
;; It handles registration with the DragonRuby Kernel and coordinates
;; the lifecycle of the mind (Emacs) and body (Ruby).

(require 'dragonruby-kernel)
(require 'dragonruby-stargate-manager)
(require 'dragonruby-stargate-tracker)
(require 'dragonruby-stargate-bridge)
(require 'dragonruby-stargate-timeline)
(require 'dragonruby-stargate-injector)

(defvar-local dragonruby-stargate--mode-line-indicator nil
  "Mode-line indicator for Stargate status.")

(put 'dragonruby-stargate--mode-line-indicator 'risky-local-variable t)

(defgroup dragonruby-stargate nil
  "Stargate: Time-traveling state management for DragonRuby."
  :group 'dragonruby)

(defun dragonruby-stargate-enable ()
  "Initialize and enable the Stargate module as a Kernel Organ."
  (interactive)
  (let ((root (dragonruby--find-project-root)))
    (if root
        (progn
          (dragonruby-stargate-session-init root)
          
          ;; Wire the Chronicler: Attach to the Kernel Pulse
          ;; The Monitor Hook is our 'Light Heartbeat' (runs every pulse)
          (dragonruby-kernel-register-hook 'dragonruby-monitor-hook 
                                         #'dragonruby-stargate--heartbeat t)
          
          ;; Wire the Surgeon: Atomic Injections on Save
          (dragonruby-kernel-register-hook 'after-save-hook 
                                         #'dragonruby-stargate-inject-buffer t)
          
          
          (setq dragonruby-stargate--mode-line-indicator
                (propertize " 🌌" 'face 'success 'help-echo "Stargate Chronicler Cabled"))
          (add-to-list 'mode-line-process '(:eval dragonruby-stargate--mode-line-indicator))
          
          (message "🚀 STARGATE: Organ active and synced with Kernel Pulse."))
      (error "Not in a DragonRuby project"))))

(defun dragonruby-stargate-disable ()
  "Deactivate and cleanup the Stargate module."
  (interactive)
  ;; Hooks registered with (local=t) and via dragonruby-kernel-register-hook
  ;; are automatically cleaned up by (dragonruby-kernel-reset-live), 
  ;; but we follow the protocol for manual disabling.
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

;; Register with the Sovereign Kernel (v0.7.3 Standard)
(dragonruby-register-module
 :name 'stargate
 :type 'feature
 :namespace "dragonruby-stargate"
 :provides '(time-travel branching state-integrity)
 :requires '(:core :scheduler)
 :entry-point 'dragonruby-stargate-core
 :enable-fn #'dragonruby-stargate-enable
 :disable-fn #'dragonruby-stargate-disable)

(defun dragonruby-stargate-inject-buffer (&optional file)
  "Inject the current buffer into the simulation with risk classification."
  (interactive)
  (let* ((code (buffer-substring-no-properties (point-min) (point-max))))
    ;; Transmission happens inside classify-and-transmit logic
    (dragonruby-stargate-injector-transmit code)
    ;; Sync the tracker
    (dragonruby-stargate-tracker-update-hash (or file (buffer-file-name)))))

(provide 'dragonruby-stargate-core)
;;; core.el ends here
