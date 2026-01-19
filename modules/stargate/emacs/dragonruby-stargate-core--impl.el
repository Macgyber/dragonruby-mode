;;; dragonruby-stargate-core--impl.el --- Stargate Private Implementation -*- lexical-binding: t -*-

;; Author: Macgyber <esteban3261g@gmail.com>
;; Version: 0.8.0

;;; Commentary:
;; This provides the internal machinery for Stargate.

;;; Code:

(require 'dragonruby-kernel)
(require 'dragonruby-utils)
(require 'dragonruby-stargate-tracker)
(require 'dragonruby-stargate-manager)
(require 'dragonruby-stargate-bridge)
(require 'dragonruby-stargate-timeline)
(require 'dragonruby-stargate-injector)
(require 'dragonruby-stargate-status)

;; Compatibility alias (Sync reality)
(defalias 'dragonruby-stargate--notify #'dragonruby--notify)

(defvar dragonruby-stargate--global-timer nil
  "Global timer for connection monitoring.")

(defvar dragonruby-stargate--state) ; Defined in manager.el

(defvar-local dragonruby--buffer-project-root :not-set
  "The anchored project root for this buffer.")

(defvar dragonruby--last-detected-project-root nil
  "The last valid project root detected globally.")

(defvar-local dragonruby-stargate--divergence-detected nil
  "Flag indicating if a state divergence has been detected in this buffer.")

(defvar dragonruby-stargate--infection-requested nil
  "Internal flag to prevent redundant leaps in a single session.")

(defvar dragonruby-stargate--mode-line-indicator " 💤"
  "Current mode-line indicator string.")

(put 'dragonruby-stargate--mode-line-indicator 'risky-local-variable t)
(put 'dragonruby-stargate--divergence-detected 'risky-local-variable t)

;;;###autoload
(defun dragonruby-stargate-enable ()
  "Initialize and enable the Stargate module.
The system checks for connections periodically."
  (interactive)
  (let ((root (dragonruby--find-project-root t)))
    ;; 1. Global Monitoring (Always needed if module is ON)
    (unless (and dragonruby-stargate--global-timer 
                 (timerp dragonruby-stargate--global-timer))
      (setq dragonruby-stargate--global-timer
            (run-with-timer 1 5 #'dragonruby-stargate--global-monitor)))
    
    ;; 2. Injections on Save (Buffer Local)
    (dragonruby-kernel-register-hook 'after-save-hook 
                                   #'dragonruby-stargate-inject-buffer t)
    
    ;; 3. Mode-line setup
    (setq dragonruby-stargate--mode-line-indicator
          (propertize " 💤" 'face 'shadow 'help-echo "Stargate: Searching for Dragon..."))
    (add-to-list 'mode-line-process '(:eval dragonruby-stargate--mode-line-indicator))
    
    ;; 4. Show status dashboard (The Architect's View)
    (dragonruby-stargate-status-buffer)
    
    (unless root
      (message "📡 STARGATE: Ready (Radar Blind - Open a main.rb to sync)"))))

;;;###autoload
(defun dragonruby-stargate-disable ()
  "Disable Stargate features and cleanup."
  (interactive)
  ;; Stop monitoring
  (when dragonruby-stargate--global-timer
    (cancel-timer dragonruby-stargate--global-timer)
    (setq dragonruby-stargate--global-timer nil))
  
  ;; Stop active sessions
  (dragonruby-stargate-session-stop)
  
  ;; Cleanup UI
  (setq mode-line-process (cl-delete '(:eval dragonruby-stargate--mode-line-indicator) mode-line-process :test 'equal))
  
  ;; Remove hooks
  (dragonruby-kernel-unregister-hook 'after-save-hook #'dragonruby-stargate-inject-buffer t)
  
  (message "💤 STARGATE: Hibernating."))

(defun dragonruby-stargate--ensure-session ()
  "INVARIANT: Ensure a valid project root and session exist.
NEVER depends on the current buffer context alone.
Returns the session path if successful, nil otherwise."
  (let ((root (or (dragonruby--find-project-root t)
                  dragonruby--last-detected-project-root)))
    (if (not root)
        (progn
          (dragonruby-stargate--set-state :radar-blind)
          nil)
      (progn
        (unless (and dragonruby-stargate--active-session 
                     dragonruby-stargate--session-index)
          (dragonruby-stargate-session-init root))
        dragonruby-stargate--active-session))))

(defun dragonruby-stargate--infect-runtime ()
  "Apply the interposition hook (Law VI).
CONTRACT: Must only be called after `dragonruby-stargate--ensure-session` returns t."
  (interactive)
  (if (not (dragonruby-stargate--ensure-session))
      (message "❌ STARGATE: Cannot infect, Radar is Blind (No Project Root).")
    (cond
     (dragonruby-stargate--infection-requested
      nil) ;; Silent return to avoid flooding
     ((eq dragonruby-stargate--state :infecting)
      nil)
     ((not (and dragonruby-stargate-bridge--process (process-live-p dragonruby-stargate-bridge--process)))
      (dragonruby-stargate--set-state nil)
      (message "❌ STARGATE: Cannot infect, bridge is dead."))
     (t
      (let* ((root (or (dragonruby--find-project-root t) (dragonruby--anchor-project-root) dragonruby--last-detected-project-root))
             (mygame-dir (expand-file-name "mygame/" root))
             (portal-file (expand-file-name "stargate_portal.rb" mygame-dir))
             (runtime-dir (expand-file-name "modules/stargate/runtime" dragonruby-mode-root))
             (init-src (expand-file-name "stargate_init.rb" runtime-dir))
             (boot-src (expand-file-name "bootstrap.rb" runtime-dir)))
        
        (unless (file-directory-p mygame-dir)
          (make-directory mygame-dir t))
        
        ;; 1. Lock the leap IMMEDIATELY
        (setq dragonruby-stargate--infection-requested t)
        
        (condition-case err
            (with-temp-file portal-file
              (set-buffer-file-coding-system 'utf-8-unix)
              (insert "# Stargate Portal (Temporary)\n")
              (insert (format "$LOAD_PATH << '%s'\n" (expand-file-name runtime-dir)))
              (insert-file-contents boot-src)
              (goto-char (point-max))
              (insert "\n")
              (insert-file-contents init-src)
              ;; Mandatory newline at end
              (insert "\n"))
          (error 
           (setq dragonruby-stargate--infection-requested nil)
           (message "❌ STARGATE: Failed to create portal: %s" (error-message-string err))))
        
        (when dragonruby-stargate--infection-requested
          (let ((ruby-cmd "begin; load 'mygame/stargate_portal.rb'; Stargate::Bootstrap.infect!; rescue Exception => e; puts \"[STARGATE_ERROR] #{e.class}: #{e.message}\"; end"))
            (dragonruby-stargate-bridge-send-code ruby-cmd))
          (setq dragonruby-stargate--infection-request-time (current-time))))))))

(defun dragonruby-stargate--global-monitor ()
  "Monitor for DragonRuby connections."
  (let* ((connected (and (processp dragonruby-stargate-bridge--process)
                         (process-live-p dragonruby-stargate-bridge--process)))
         (has-session (and dragonruby-stargate--active-session 
                          dragonruby-stargate--session-index)))
    
    (if connected
        ;; Case A: Connected
        (unless (or (eq dragonruby-stargate--state :active)
                    (eq dragonruby-stargate--state :infecting)
                    dragonruby-stargate--runtime-infected)
          ;; SUPREME INVARIANT:
          ;; Never infect without evaluating ensure-session first.
          (when (dragonruby-stargate--ensure-session)
            (dragonruby-stargate--infect-runtime)))
      
      ;; Case B: Not connected
      (progn
        (when dragonruby-stargate--state
          (unless (and dragonruby-stargate-bridge--process 
                       (process-live-p dragonruby-stargate-bridge--process))
            (dragonruby-stargate-session-stop)))
        
        (when (dragonruby-stargate-bridge-find-and-install t)
          (setq connected t))))
    
    ;; Update indicator aesthetics in all buffers locally
    (let* ((diverged (seq-some (lambda (b) (buffer-local-value 'dragonruby-stargate--divergence-detected b)) (buffer-list)))
           (infected (and connected dragonruby-stargate--runtime-infected))
           (timeout (and connected 
                         (not infected)
                         dragonruby-stargate--infection-request-time
                         (> (float-time (time-subtract (current-time) dragonruby-stargate--infection-request-time)) 30.0)))
           (state dragonruby-stargate--state) ; Get current state
           (char (cond ((not connected) " 💤")
                       (diverged " ⚡")
                       (timeout " ⚠️")
                       (infected " 🌌")
                       (t " 💤")))
           (face (cond ((not connected) 'shadow)
                       (diverged 'error)
                       (timeout 'error)
                       (infected 'success)
                       ((eq state :radar-blind) 'warning) ; Added radar-blind state
                       (t 'warning)))
           (help (cond ((not connected) "Searching for Dragon...")
                       (diverged "Divergence Detected! Jump back to stabilize.")
                       (timeout "Infection Timeout: DragonRuby is not responding to Stargate hook.")
                       (infected "Stargate Active: Cabled & Syncing.")
                       ((eq state :radar-blind) "Radar Blind: No project root detected. Open a main.rb.") ; Added radar-blind help
                       (t "Connected: Waiting for DragonRuby tick to interpose..."))))
      
      (when timeout
        (message "📡 STARGATE: Infection Timeout! Re-trying...")
        (setq dragonruby-stargate--infection-requested nil) ;; Unlock for retry
        (dragonruby-stargate--set-state nil)
        (setq dragonruby-stargate--infection-request-time nil))

      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when dragonruby-mode
            (setq dragonruby-stargate--mode-line-indicator 
                  (propertize char 'face face 'help-echo help)))))
      
      ;; Refresh dashboard if visible
      (when (fboundp 'dragonruby-stargate-status-refresh)
        (dragonruby-stargate-status-refresh)))))

(defun dragonruby-stargate-record ()
  "Start or resume Stargate recording/synchronization.
If the runtime is not yet infected, triggers infection first."
  (interactive)
  (unless (and dragonruby-stargate-bridge--process (process-live-p dragonruby-stargate-bridge--process))
    (dragonruby-stargate-bridge-find-and-install))
  
  (cond
   ((eq dragonruby-stargate--state :active)
    (dragonruby-stargate-bridge-send-code "Stargate::Clock.resume!")
    (message "🔴 STARGATE: Recording/Sync Active."))
   ((eq dragonruby-stargate--state :infecting)
    (message "🔭 STARGATE: Infection in progress. Wait for ACK..."))
   (t
    (dragonruby-stargate--infect-runtime)
    (message "🔭 STARGATE: Waiting for interposition to start recording..."))))

(defun dragonruby-stargate-pause ()
  "Pause Stargate recording/synchronization (Stasis)."
  (interactive)
  (dragonruby-stargate-bridge-send-code "Stargate::Clock.pause!")
  (message "🛑 STARGATE: Paused (Stasis)."))

;;;###autoload
(defun dragonruby-stargate-jump ()
  "Jump to a specific moment. Opens the timeline if no address is provided."
  (interactive)
  (if (fboundp 'dragonruby-stargate-timeline)
      (dragonruby-stargate-timeline)
    (message "Stargate Timeline module not loaded.")))

(provide 'dragonruby-stargate-core--impl)
;;; dragonruby-stargate-core--impl.el ends here
