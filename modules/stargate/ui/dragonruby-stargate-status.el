;;; dragonruby-stargate-status.el --- Stargate Status Dashboard -*- lexical-binding: t -*-

;; Author: Macgyber <esteban3261g@gmail.com>
;; Version: 0.8.0

;;; Commentary:
;; Mini-Dashboard for Stargate - shows current state without noise.

;;; Code:

(defvar dragonruby-stargate-status-buffer "*DragonRuby Status*"
  "Buffer name for the Stargate status dashboard.")

(defvar dragonruby-stargate-status-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'dragonruby-stargate-status-refresh)
    (define-key map (kbd "i") #'dragonruby-stargate--infect-runtime)
    map)
  "Keymap for Stargate status dashboard.")

(define-derived-mode dragonruby-stargate-status-mode special-mode "Stargate-Status"
  "Major mode for the Stargate status dashboard."
  (setq buffer-read-only t))

(defun dragonruby-stargate-status-buffer ()
  "Show the Stargate status dashboard."
  (interactive)
  (let ((buf (get-buffer-create dragonruby-stargate-status-buffer)))
    (with-current-buffer buf
      (dragonruby-stargate-status-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dragonruby-stargate-status--render)
        (goto-char (point-min))))
    (display-buffer buf)
    buf))

(defun dragonruby-stargate-status--render ()
  "Render the status dashboard content."
  (let* ((connected (and (processp dragonruby-stargate-bridge--process)
                         (process-live-p dragonruby-stargate-bridge--process)))
         (state dragonruby-stargate--state)
         (infected dragonruby-stargate--runtime-infected)
         (session dragonruby-stargate--active-session)
         (moments (when dragonruby-stargate--session-index
                   (hash-table-count (cdr (assoc "moments" dragonruby-stargate--session-index))))))
    
    (insert (propertize "🐉 DragonRuby — Stargate Status\n" 'face '(:height 1.3 :weight bold)))
    (insert (propertize (make-string 50 ?─) 'face 'shadow) "\n\n")
    
    ;; Stargate Status
    (insert (propertize "Stargate Systems\n" 'face '(:weight bold)))
    (insert (format "  Status      : %s\n"
                    (cond
                     ((eq state :active) (propertize "🟢 Active & Syncing" 'face 'success))
                     ((eq state :infecting) (propertize "🔄 Infecting (Interposing)..." 'face 'warning))
                     ((eq state :radar-blind) (propertize "⚫ Radar Blind (Missing Project Root)" 'face 'warning))
                     (connected (propertize "💤 Connected & Cabled" 'face 'shadow))
                     (t (propertize "⚫ No Connection" 'face 'shadow)))))
    
    (insert (format "  Engine ID   : %s\n" 
                    (if connected 
                        (propertize (number-to-string (process-id dragonruby-stargate-bridge--process)) 'face 'font-lock-constant-face)
                      (propertize "---" 'face 'shadow))))

    (insert (format "  Monitor     : %s\n"
                    (if (and dragonruby-stargate--global-timer
                             (timerp dragonruby-stargate--global-timer))
                        (propertize "✔ Global Sweep Active" 'face 'success)
                      (propertize "✘ Inactive" 'face 'error))))
    
    (insert (format "  Session     : %s\n"
                    (if session
                        (propertize (file-name-nondirectory (directory-file-name session)) 'face 'font-lock-string-face)
                      (propertize "None" 'face 'shadow))))
    
    (when moments
      (insert (format "  Frames      : %s\n" (propertize (number-to-string moments) 'face 'font-lock-constant-face))))
    
    (insert "\n")
    
    ;; Hints
    (insert (propertize "Operational Controls\n" 'face '(:weight bold)))
    (insert "  • " (propertize "i" 'face 'font-lock-keyword-face) " : Manual Infection (Force Cable)\n")
    (insert "  • " (propertize "F7" 'face 'font-lock-keyword-face) ": Start/Resume Recording\n")
    (insert "  • " (propertize "F8" 'face 'font-lock-keyword-face) ": Pause (Stasis)\n")
    (insert "  • " (propertize "F9" 'face 'font-lock-keyword-face) ": Open Temporal Timeline\n")
    (insert "  • " (propertize "g" 'face 'font-lock-keyword-face) " : Refresh Radar\n")
    (insert "  • " (propertize "q" 'face 'font-lock-keyword-face) " : Close Console\n")
    
    (insert "\n")
    (insert (propertize (make-string 50 ?─) 'face 'shadow) "\n")))

(defun dragonruby-stargate-status-refresh ()
  "Refresh the status dashboard if it's visible."
  (when (get-buffer dragonruby-stargate-status-buffer)
    (with-current-buffer dragonruby-stargate-status-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dragonruby-stargate-status--render)
        (goto-char (point-min))))))

;; Auto-refresh when Stargate state changes
(add-hook 'dragonruby-stargate-bridge-event-hook #'dragonruby-stargate-status-refresh)

(provide 'dragonruby-stargate-status)
;;; dragonruby-stargate-status.el ends here
