;;; dragonruby-stargate-telemetry.el --- Stargate Event Processing -*- lexical-binding: t -*-

(require 'dragonruby-stargate-fsm)
(require 'dragonruby-stargate-sessions)

(defvar dragonruby-stargate--event-queue nil "Incoming telemetry from the bridge.")
(defvar dragonruby-stargate--active-address nil "Current branch@frame.")
(defvar dragonruby-stargate--runtime-ready nil "ACK flag from DragonRuby.")

(defun dragonruby-stargate-telemetry--process ()
  "Process the batch of events received since last pulse."
  (when dragonruby-stargate--event-queue
    (let ((batch (reverse dragonruby-stargate--event-queue)))
      (setq dragonruby-stargate--event-queue nil)
      (dolist (e batch)
        (let ((type (cdr (assoc "type" e))))
          (cond
           ((string= type "moment") (dragonruby-stargate-telemetry--record-moment e))
           ((string= type "ready") (setq dragonruby-stargate--runtime-ready t) (dragonruby-stargate--set-state :active))
           ((string= type "stasis_ack") (message "🛑 STARGATE: STASIS ACTIVE."))
           ((string= type "record_ack") (message "🔴 STARGATE: RECORDING ACTIVE."))))))
    (run-hooks 'dragonruby-stargate-session-updated-hook))

  ;; Auto-persist every 30 seconds
  (let ((now (float-time)))
    (when (> (- now (or (bound-and-true-p dragonruby-stargate--last-persist-time) 0)) 30.0)
      (dragonruby-stargate-session-persist)
      (setq dragonruby-stargate--last-persist-time now))))

(defun dragonruby-stargate-telemetry--record-moment (e)
  "Update the session index with a new moment."
  (let* ((addr (cdr (assoc "address" e)))
         (branch (car (split-string addr "@")))
         (index dragonruby-stargate--session-index)
         (moments (cdr (assoc "moments" index)))
         (bmaps (cdr (assoc "branch-maps" index))))
    (puthash addr (list (cons "hash" (cdr (assoc "hash" e))) (cons "seed" (cdr (assoc "seed" e)))) moments)
    (setq dragonruby-stargate--active-address addr)
    (puthash branch (append (gethash branch bmaps) (list addr)) bmaps)))

(defun dragonruby-stargate-session-get-moment (branch frame)
  "Retrieve moment data for a specific BRANCH and FRAME."
  (when dragonruby-stargate--session-index
    (let* ((moments (cdr (assoc "moments" dragonruby-stargate--session-index)))
           (addr (format "%s@%d" branch frame)))
      (gethash addr moments))))

(defun dragonruby-stargate-session-enqueue (event)
  "Push EVENT into the queue."
  (push event dragonruby-stargate--event-queue))

(add-hook 'dragonruby-stargate-bridge-event-hook #'dragonruby-stargate-session-enqueue)
(add-hook 'dragonruby-monitor-hook #'dragonruby-stargate-telemetry--process)

(provide 'dragonruby-stargate-telemetry)
