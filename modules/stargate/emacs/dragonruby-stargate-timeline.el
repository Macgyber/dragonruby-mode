;;; dragonruby-stargate-timeline.el --- Stargate Temporal Controller -*- lexical-binding: t -*-

(require 'dragonruby-stargate-recorder)
(require 'dragonruby-stargate-sessions)
(require 'dragonruby-stargate-telemetry)

(defvar dragonruby-stargate-timeline--last-jump nil
  "The target address of the last restoration intent.")

(defun dragonruby-stargate-timeline-jump-at-point ()
  "Express restoration intent for the moment at point."
  (interactive)
  (let ((address (get-text-property (point) 'address)))
    (if address
        (dragonruby-stargate-timeline-scrub address)
      (message "No moment at cursor."))))

(defun dragonruby-stargate-timeline-scrub (address)
  "Send a restoration intent for ADDRESS."
  (interactive "sAddress: ")
  (unless (and dragonruby-stargate--active-session dragonruby-stargate--runtime-ready)
    (error "Stargate: Analyzer requiring live runtime"))

  (let* ((parts (split-string address "@"))
         (branch (nth 0 parts))
         (frame (string-to-number (nth 1 parts)))
         (moment (dragonruby-stargate-session-get-moment branch frame)))
    
    (if moment
        (let* ((hash (cdr (assoc "hash" moment)))
               (seed (cdr (assoc "seed" moment))))
          (setq dragonruby-stargate-timeline--last-jump address)
          (message "⏳ Stargate: Intent -> RESTORE %s" address)
          (dragonruby-stargate--send-active-command 
           (format "Stargate::Timeline.restore!(%S, %s, %S, %s)" 
                   branch frame hash seed)))
      (error "Moment definition missing: %s" address))))

(defun dragonruby-stargate-timeline-fork ()
  "Fork a new branch from point."
  (interactive)
  (let ((address (get-text-property (point) 'address)))
    (if address
        (let* ((parts (split-string address "@"))
               (parent-id (car parts))
               (frame (string-to-number (cadr parts)))
               (moment (dragonruby-stargate-session-get-moment parent-id frame))
               (hash (and moment (cdr (assoc "hash" moment)))))
          (if (not hash)
              (error "Hash verification failed for %s" address)
            (message "🌱 Stargate: Intent -> FORK from %s@%d" parent-id frame)
            (dragonruby-stargate--send-active-command 
             (format "Stargate::Timeline.branch!(%d, %S, %S)" frame parent-id hash))))
      (message "No moment selected."))))

(provide 'dragonruby-stargate-timeline)
