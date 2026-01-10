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
  "Hook run during the processing phase.
Modules should register their synchronous scan functions here.
Functions must be safe and should not perform their own redisplays.")

;; --- CONFIGURATION ---

(defcustom dragonruby-idle-delay 0.2
  "Seconds of idle time before triggering a scan pulse."
  :type 'float
  :group 'dragonruby)

;; --- DEBUGGING ---

(defvar dragonruby-scheduler-debug nil
  "If non-nil, log scheduler state transitions to *Messages*.")

(defun dragonruby--log (msg &rest args)
  "Log MSG with ARGS if debugging is enabled."
  (when dragonruby-scheduler-debug
    (message (concat " [DR-SCHED] " msg) (apply #'format args))))

;; --- PULSE LOGIC ---

(defun dragonruby-pulse ()
  "The single orchestration event.
1. Checks if buffer is ready.
2. Transitions state to 'processing.
3. Runs all hooks in `dragonruby-scan-hook` safely.
4. Restores state to 'idle."
  ;; Only proceed if we are in a valid buffer and state
  (when (and (buffer-live-p (current-buffer))
             (eq dragonruby--buffer-state 'scheduled))
    
    (dragonruby--log "PULSE START | State: scheduled -> processing")
    
    ;; 1. Transition to Processing
    (setq dragonruby--buffer-state 'processing)
    (setq dragonruby--idle-timer nil) ;; Timer has fired, clear it

    ;; 2. Execution Phase
    (let ((start-time (float-time)))
      (condition-case err
          (run-hooks 'dragonruby-scan-hook)
        (error
         (message "DragonRuby Pulse Critical Error: %s" err)))
      
      ;; Force visual update immediately
      (when (get-buffer-window (current-buffer))
        (redisplay))
        
      (dragonruby--log "PULSE END   | State: processing -> idle | Duration: %.4fs" 
                       (- (float-time) start-time)))
    
    ;; 3. Transition back to Idle
    (setq dragonruby--buffer-state 'idle)))

(defun dragonruby--schedule-pulse ()
  "Schedule a pulse if one isn't already pending."
  (unless (memq dragonruby--buffer-state '(scheduled processing))
    (dragonruby--log "Schedule Pulse | State: %s -> scheduled" dragonruby--buffer-state)
    
    ;; Mark as scheduled
    (setq dragonruby--buffer-state 'scheduled)
    
    ;; Cancel existing timer just in case (defensive)
    (when dragonruby--idle-timer
      (cancel-timer dragonruby--idle-timer))
    
    ;; Schedule new timer
    (setq dragonruby--idle-timer
          (run-with-idle-timer dragonruby-idle-delay nil
                               (lambda (buf)
                                 (when (buffer-live-p buf)
                                   (with-current-buffer buf
                                     (dragonruby-pulse))))
                               (current-buffer)))))

;; --- HOOKS ---

(defun dragonruby--on-change (beg end len)
  "Global handler for buffer changes.
Marks buffer dirty and interacts with scheduler."
  ;; We don't care about the specific change details, only that it happened.
  (unless (eq dragonruby--buffer-state 'processing)
    ;; Only log if not already dirty to avoid spamming logs on every keystroke
    (unless (eq dragonruby--buffer-state 'dirty)
      (dragonruby--log "Buffer Change  | State: %s -> dirty" dragonruby--buffer-state))
      
    (setq dragonruby--buffer-state 'dirty)
    (dragonruby--schedule-pulse)))

;; --- PUBLIC API ---

(defun dragonruby-scheduler-enable ()
  "Enable the scheduler for the current buffer."
  (dragonruby--log "ENABLE")
  (setq dragonruby--buffer-state 'idle)
  (add-hook 'after-change-functions #'dragonruby--on-change nil t)
  ;; Trigger initial scan
  (dragonruby--schedule-pulse))

(defun dragonruby-scheduler-disable ()
  "Disable scheduler and cleanup."
  (dragonruby--log "DISABLE")
  (setq dragonruby--buffer-state 'clean)
  (remove-hook 'after-change-functions #'dragonruby--on-change t)
  (when dragonruby--idle-timer
    (cancel-timer dragonruby--idle-timer)
    (setq dragonruby--idle-timer nil)))

(provide 'dragonruby-scheduler)
