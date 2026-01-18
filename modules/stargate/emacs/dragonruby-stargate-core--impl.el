;;; dragonruby-stargate-core--impl.el --- Stargate Main Orchestrator implementation -*- lexical-binding: t -*-

;; Author: Macgyber <esteban3261g@gmail.com>
;; Version: 0.8.0
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/Macgyber/dragonruby-mode

;;; Commentary:
;; Internal implementation of Stargate enable/disable and background monitoring.

;;; Code:

(require 'dragonruby-kernel)
(require 'dragonruby-stargate-manager)
(require 'dragonruby-stargate-tracker)
(require 'dragonruby-stargate-bridge)
(require 'dragonruby-stargate-timeline)
(require 'dragonruby-stargate-injector)

(defvar dragonruby-stargate--global-timer nil
  "Global timer for connection monitoring.")

(defvar-local dragonruby-stargate--mode-line-indicator nil
  "Mode-line indicator for Stargate status.")

(put 'dragonruby-stargate--mode-line-indicator 'risky-local-variable t)

;;;###autoload
(defun dragonruby-stargate-enable ()
  "Initialize and enable the Stargate module.
The system checks for connections periodically."
  (interactive)
  (let ((root (dragonruby--find-project-root)))
    (if root
        (progn
          ;; 1. Global Monitoring (Once per Emacs instance)
          (unless (and dragonruby-stargate--global-timer 
                       (timerp dragonruby-stargate--global-timer))
            (setq dragonruby-stargate--global-timer
                  (run-with-timer 1 3 #'dragonruby-stargate--global-monitor)))
          
          ;; 2. Injections on Save (Buffer Local)
          (dragonruby-kernel-register-hook 'after-save-hook 
                                         #'dragonruby-stargate-inject-buffer t)
          
          (setq dragonruby-stargate--mode-line-indicator
                (propertize " 💤" 'face 'shadow 'help-echo "Stargate: Searching for Dragon..."))
          (add-to-list 'mode-line-process '(:eval dragonruby-stargate--mode-line-indicator))
          
          (message "🚀 STARGATE: Ready in [%s]. (Global Monitor started every 3s)" root))
      (error "STARGATE: Unable to find DragonRuby project root"))))

;;;###autoload
(defun dragonruby-stargate-disable ()
  "Deactivate and cleanup the Stargate module."
  (interactive)
  (when dragonruby-stargate--global-timer
    (cancel-timer dragonruby-stargate--global-timer)
    (setq dragonruby-stargate--global-timer nil))
  (dragonruby-stargate-session-stop)
  (remove-hook 'after-save-hook #'dragonruby-stargate-inject-buffer t)
  ;; Cleanup mode-line
  (setq mode-line-process 
        (cl-remove-if (lambda (x) 
                        (and (listp x) 
                             (eq (car x) :eval) 
                             (string-match-p "dragonruby-stargate--mode-line-indicator" 
                                             (format "%s" (cadr x)))))
                      mode-line-process))
  (setq dragonruby-stargate--mode-line-indicator nil)
  (message "💤 STARGATE: Global systems shut down."))

(defun dragonruby-stargate--global-monitor ()
  "Monitor for DragonRuby connections."
  (let* ((connected (and (processp dragonruby-stargate-bridge--process)
                         (process-live-p dragonruby-stargate-bridge--process)))
         (has-session (and dragonruby-stargate--active-session 
                          dragonruby-stargate--session-index)))
    
    (if connected
        ;; Case A: Connected, check if we need a session
        (unless has-session
          (let ((root (dragonruby--find-project-root)))
            (when root
              (dragonruby-stargate-session-init root))))
      
      ;; Case B: Not connected, try to find it
      (when (dragonruby-stargate-bridge-find-and-install t)
        ;; Found! Monitor logic will handle session on next tick (3s)
        ;; This avoids recursive CPU spikes.
        (setq connected t))
      
      ;; If still not found and we had a session, it's stale now.
      (when (and (not connected) has-session)
        (dragonruby-stargate-session-stop)))
    
    ;; Update indicator aesthetics in all buffers locally
    (let ((char (if connected " 🌌" " 💤"))
          (face (if connected 'success 'shadow))
          (help (if connected "Cabled" "Searching...")))
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (boundp 'dragonruby-stargate--mode-line-indicator)
            (setq dragonruby-stargate--mode-line-indicator
                  (propertize char 'face face 'help-echo help))))))))

;;;###autoload
(defun dragonruby-stargate-inject-buffer (&optional file)
  "Inject the current buffer into the simulation with risk classification."
  (interactive)
  (let* ((code (buffer-substring-no-properties (point-min) (point-max))))
    (dragonruby-stargate-injector-transmit code)
    (dragonruby-stargate-tracker-update-hash (or file (buffer-file-name)))))

(provide 'dragonruby-stargate-core--impl)
;;; dragonruby-stargate-core--impl.el ends here
