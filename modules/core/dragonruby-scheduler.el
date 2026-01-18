;;; dragonruby-scheduler.el --- Centralized Event Loop & State Machine -*- lexical-binding: t; -*-

(require 'dragonruby-core)

;; --- STATE MACHINE ---

(defvar-local dragonruby--buffer-state 'idle
  "Current lifecycle state of the buffer.
Possible values:
- 'clean:      No pending changes.
- 'dirty:      Content changed, waiting for schedule.
- 'scheduled:  Timer active, waiting for pulse.
- 'processing: Currently running analysis.
- 'idle:       Ready for next event.")

(defvar-local dragonruby--idle-timer nil
  "The single authoritative idle timer for this buffer.")

(defvar dragonruby-scan-hook nil
  "Heavy tasks: Run during processing after buffer changes.
Modules register synchronous analysis here.")

(defvar dragonruby-monitor-hook nil
  "Light tasks: Run during every idle pulse (even if clean).
Ideal for UI updates, hover checks, and ephemeral popups.")

;; --- CONFIGURATION ---

(defcustom dragonruby-idle-delay 1.5
  "Seconds of idle time before triggering a scan pulse."
  :type 'float
  :group 'dragonruby)

(defcustom dragonruby-popup-trigger-delay 0.2
  "Seconds to wait before showing sprite popup after hover."
  :type 'number
  :group 'dragonruby)

(defcustom dragonruby-popup-monitor-interval 0.25
  "Seconds between popup monitor checks."
  :type 'number
  :group 'dragonruby)

(defun dragonruby-scheduler--calculate-delay ()
  "Calculate adaptive debounce delay based on buffer size.
Large buffers get a longer delay to protect the CPU."
  (let* ((size (buffer-size))
         (base 0.8)
         ;; Scale: +0.2s for every 50kb
         (extra (* (/ size 50000.0) 0.2)))
    (min 3.0 (+ base extra))))

(defvar dragonruby-scan-debounce-delay 0.6
  "OBSOLETE: Use (dragonruby-scheduler--calculate-delay) instead.")

;; --- DEBUGGING ---

(defvar dragonruby-scheduler-debug nil
  "If non-nil, log scheduler state transitions to *Messages*.")

(defun dragonruby--log (msg &rest args)
  "Log MSG with ARGS if debugging is enabled."
  (when dragonruby-scheduler-debug
    (message (concat " [DR-SCHED] " msg) (apply #'format args))))

;; --- PULSE LOGIC ---

;; --- STATE ---

(defvar-local dragonruby--last-scan-time 0
  "Timestamp of the last heavy scan.")

(defvar-local dragonruby--last-interaction-time 0
  "Timestamp of the last user interaction (change).")

;; --- PULSE LOGIC ---

(defvar-local dragonruby--pulse-in-progress nil
  "Guard to prevent recursive or overlapping pulses.")

(defun dragonruby-pulse (buf)
  "The single authoritative heart pulse for BUF.
Governs both 'scan' (heavy) and 'monitor' (light) phases."
  (when (and (buffer-live-p buf) (not dragonruby--pulse-in-progress))
    (setq dragonruby--pulse-in-progress t)
    (unwind-protect
        (with-current-buffer buf
          ;; 1. Global Guard: No Project, No Pulse.
          (let ((root (dragonruby--find-project-root)))
            (when (and root 
                       dragonruby-mode 
                       (get-buffer-window buf t)
                       (or dragonruby-scan-hook dragonruby-monitor-hook))
              
              (let ((now (float-time)))
                ;; 1. Monitor Phase (Fast/Light)
              (dolist (fn dragonruby-monitor-hook)
                (when (functionp fn)
                  (condition-case err (funcall fn)
                    (error (message " [DR-SCHED] Monitor Error (%s): %s" fn err)))))

              ;; 2. Scan Phase (Slow/Heavy)
              (when (eq dragonruby--buffer-state 'dirty)
                (let* ((idle-since-change (- now dragonruby--last-interaction-time))
                       (adaptive-delay (dragonruby-scheduler--calculate-delay)))
                  (when (>= idle-since-change adaptive-delay)
                    (dragonruby--log "PULSE SCAN START | Idle: %.2fs (Adaptive Delay: %.2fs)" 
                                    idle-since-change adaptive-delay)
                    (setq dragonruby--buffer-state 'processing)
                    (let ((range (dragonruby--visible-region)))
                      (dolist (fn dragonruby-scan-hook)
                        (when (functionp fn)
                          (condition-case err (funcall fn (car range) (cdr range))
                            (error (message " [DR-SCHED] Scan Error (%s): %s" fn err))))))
                    
                    (setq dragonruby--last-scan-time now)
                    (setq dragonruby--buffer-state 'clean)
                    (dragonruby--log "PULSE SCAN END"))))))))
      (setq dragonruby--pulse-in-progress nil))))

(defun dragonruby--schedule-pulse ()
  "Ensure the heartbeat is scheduled for the next idle moment."
  (when (and (bound-and-true-p dragonruby-mode)
             (not (and dragonruby--idle-timer 
                       (timerp dragonruby--idle-timer)
                       (memq dragonruby--idle-timer timer-idle-list))))
    (setq dragonruby--idle-timer
          (dragonruby-kernel-register-timer
           (run-with-idle-timer 0.5 nil
                                #'(lambda (buf)
                                    (when (buffer-live-p buf)
                                      (with-current-buffer buf
                                        (dragonruby-pulse buf)
                                        (setq dragonruby--idle-timer nil)
                                        (dragonruby--schedule-pulse))))
                                (current-buffer))))))

;; --- HOOKS ---

(defun dragonruby--on-change (_beg _end _len)
  "Kernel hearing the buffer scream. Marks it dirty and records time."
  (setq dragonruby--last-interaction-time (float-time))
  (setq dragonruby--buffer-state 'dirty))

;; --- PUBLIC API ---

(defun dragonruby-scheduler-enable ()
  "Enable the kernel heartbeat for the current buffer."
  ;; Start as 'dirty' to trigger an initial scan on first pulse
  (setq dragonruby--buffer-state 'dirty)
  (setq dragonruby--last-interaction-time (float-time))
  (dragonruby-kernel-register-hook 'after-change-functions #'dragonruby--on-change t)
  ;; Guard against double scheduler/timer creation
  (unless (and dragonruby--idle-timer (timerp dragonruby--idle-timer))
    (dragonruby--schedule-pulse)))

(defun dragonruby-scheduler-disable ()
  "Kill the heartbeat. Silence."
  (remove-hook 'after-change-functions #'dragonruby--on-change t)
  (when dragonruby--idle-timer
    (cancel-timer dragonruby--idle-timer)
    (setq dragonruby--idle-timer nil))
  (setq dragonruby--buffer-state 'idle))

(dragonruby-register-module
 :name 'scheduler
 :type :main
 :namespace "dragonruby-scheduler-"
 :provides '(:scheduler)
 :requires '(:core)
 :entry-point 'dragonruby-scheduler
 :enable-fn #'dragonruby-scheduler-enable
 :disable-fn #'dragonruby-scheduler-disable)

(provide 'dragonruby-scheduler)
;;; dragonruby-scheduler.el ends here
