;;; manager.el --- Stargate Session Management and Asset Vault -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'json)

(defvar json-encoding-object-type)

(defvar dragonruby-stargate--active-session nil
  "Path to the currently active .dr-stargate session directory.")

(defvar dragonruby-stargate--session-index nil
  "In-memory cache of the current session index (Hash Table based).")

(defvar dragonruby-stargate--event-queue nil
  "Queue of pending events to be processed during idle time.")

(defvar dragonruby-stargate--persist-timer nil
  "Timer for background persistence of the session index.")

(defvar dragonruby-stargate--idle-timer nil
  "Timer for processing the event queue.")

(defun dragonruby-stargate-session-init (&optional project-root)
  "Initialize a new Stargate session in PROJECT-ROOT.
If PROJECT-ROOT is nil, attempt to find it automatically."
  (interactive)
  (let* ((root (or project-root (dragonruby--find-project-root)))
         (stargate-dir (expand-file-name ".stargate/" root))
         (session-dir (expand-file-name (format-time-string "session-%Y%m%d-%H%M%S/") stargate-dir))
         (blobs-dir (expand-file-name "blobs/" session-dir)))
    (unless root (error "Could not find DragonRuby project root"))
    (unless (file-exists-p blobs-dir)
      (make-directory blobs-dir t))
    (setq dragonruby-stargate--active-session session-dir)
    
    ;; Initialize the in-memory index using Hash Tables for O(1) performance
    (let ((moments (make-hash-table :test 'equal :size 5000))
          (branches (make-hash-table :test 'equal))
          (branch-maps (make-hash-table :test 'equal)))
      ;; Initial branch
      (puthash "prime" '((parent . nil) (divergence . 0)) branches)
      (puthash "prime" '() branch-maps)
      
      (setq dragonruby-stargate--session-index
            (list (cons "version" "1.0.0")
                  (cons "metadata" (list (cons "observed_at" (float-time))))
                  (cons "branches" branches)
                  (cons "moments" moments)
                  (cons "branch-maps" branch-maps))))
    
    (dragonruby-stargate-session-persist)
    
    ;; Start background persistence (every 30 seconds, only when idle)
    (when dragonruby-stargate--persist-timer
      (cancel-timer dragonruby-stargate--persist-timer))
    (setq dragonruby-stargate--persist-timer
          (run-with-idle-timer 30 t #'dragonruby-stargate-session-persist))
    
    ;; Start Idle Event Processor
    (when dragonruby-stargate--idle-timer
      (cancel-timer dragonruby-stargate--idle-timer))
    ;; Process every 0.1s when idle, batching events.
    (setq dragonruby-stargate--idle-timer
          (run-with-idle-timer 0.2 t #'dragonruby-stargate-session-process-queue))
    
    (message "🌌 Stargate: Session initialized (High-Performance Mode)")))

(defun dragonruby-stargate-session-persist (&optional force)
  "Persist the in-memory session index to disk.
Converts hash tables to alists only for the write-out, preserving the global hash tables.
If FORCE is non-nil, ignore size limits and user input status."
  (when (and dragonruby-stargate--active-session 
             dragonruby-stargate--session-index
             (or force (not (input-pending-p)))) ;; Never block user input unless forced
    (let* ((moments-hash (cdr (assoc "moments" dragonruby-stargate--session-index)))
           (branches-hash (cdr (assoc "branches" dragonruby-stargate--session-index)))
           (bmaps-hash (cdr (assoc "branch-maps" dragonruby-stargate--session-index))))
      
      ;; Throttling: Only persist in background if the index isn't pathologically large
      (when (or force (< (hash-table-count moments-hash) 10000))
        (let ((index-file (expand-file-name "session.json" dragonruby-stargate--active-session))
              (moments-alist nil)
              (branches-alist nil)
              (bmaps-alist nil))
          
          ;; Build temporary alists for JSON
          (maphash (lambda (k v) (push (cons k v) moments-alist)) moments-hash)
          (maphash (lambda (k v) (push (cons k v) branches-alist)) branches-hash)
          (maphash (lambda (k v) (push (cons k v) bmaps-alist)) bmaps-hash)
          
          (let ((json-data (list (cons "version" "1.0.0")
                                 (cons "metadata" (cdr (assoc "metadata" dragonruby-stargate--session-index)))
                                 (cons "branches" branches-alist)
                                 (cons "moments" moments-alist)
                                 (cons "branch-maps" bmaps-alist))))
            (with-temp-file index-file
              (let ((json-encoding-object-type 'alist))
                (insert (json-encode json-data))))))))))

(defun dragonruby-stargate-session-handle-event (event)
  "Queue Stargate EVENT for asynchronous processing.
Exceptional events (divergence) bypass the queue for immediate action."
  (let ((type (cdr (assoc "type" event))))
    (if (string= type "divergence")
        (progn
          (setq dragonruby-stargate--event-queue nil) ;; Clear queue on disaster
          (dragonruby-stargate-session-record-divergence event))
      (push event dragonruby-stargate--event-queue))))

(defvar dragonruby-stargate-session-updated-hook nil
  "Hook run after a batch of events has been processed.")

(defun dragonruby-stargate-session-process-queue ()
  "Consume and process queued events during idle time."
  (when dragonruby-stargate--event-queue
    (let ((batch (reverse dragonruby-stargate--event-queue)))
      (setq dragonruby-stargate--event-queue nil)
      (dolist (event batch)
        (let ((type (cdr (assoc "type" event))))
          (cond
           ((string= type "moment") (dragonruby-stargate-session-record-moment event))
           ((string= type "branch") (dragonruby-stargate-session-record-branch event)))))
      ;; Trigger UI refresh hooks
      (run-hooks 'dragonruby-stargate-session-updated-hook))))

(defun dragonruby-stargate-session-record-moment (event)
  "Record an incoming Stargate moment from EVENT into the hash table."
  (let* ((address (cdr (assoc "address" event)))
         (hash (cdr (assoc "hash" event)))
         (seed (cdr (assoc "seed" event)))
         (moment-type (cdr (assoc "moment_type" event)))
         (observed-at (cdr (assoc "observed_at" event))))
    
    (when (and dragonruby-stargate--active-session dragonruby-stargate--session-index)
      (let ((moments (cdr (assoc "moments" dragonruby-stargate--session-index)))
            (bmaps (cdr (assoc "branch-maps" dragonruby-stargate--session-index)))
            (branch-id (car (split-string address "@"))))
        ;; 1. Store global metadata
        (puthash address `((hash . ,hash)
                           (seed . ,seed)
                           (moment_type . ,moment-type)
                           (observed_at . ,observed-at))
                 moments)
        ;; 2. Update branch-local map for fast rendering
        (let ((existing (gethash branch-id bmaps)))
          (puthash branch-id (cons address existing) bmaps))))))

(defun dragonruby-stargate-session-record-branch (event)
  "Record an incoming Stargate branch from EVENT into the hash table."
  (let* ((id (cdr (assoc "id" event)))
         (parent (cdr (assoc "parent" event)))
         (divergence (cdr (assoc "divergence" event))))
    
    (when (and dragonruby-stargate--active-session dragonruby-stargate--session-index)
      (let ((branches (cdr (assoc "branches" dragonruby-stargate--session-index))))
        (puthash id `((parent . ,parent) (divergence . ,divergence)) branches)))))

(defun dragonruby-stargate-session-record-divergence (event)
  "Handle a divergence event immediately."
  (let ((address (cdr (assoc "address" event)))
        (expected (cdr (assoc "expected" event)))
        (actual (cdr (assoc "actual" event))))
    (message "⚡ STARGATE: DIVERGENCE DETECTED at %s!" address)
    (message "   Expected: %s | Actual: %s" expected actual)
    ;; Refresh timeline immediately on divergence
    (dragonruby-stargate-timeline)))

(defun dragonruby-stargate-session-stop ()
  "Stop the current Stargate session and cleanup timers."
  (when dragonruby-stargate--persist-timer
    (cancel-timer dragonruby-stargate--persist-timer)
    (setq dragonruby-stargate--persist-timer nil))
  (when dragonruby-stargate--idle-timer
    (cancel-timer dragonruby-stargate--idle-timer)
    (setq dragonruby-stargate--idle-timer nil))
  (dragonruby-stargate-session-persist t) ;; Force final persistence
  (setq dragonruby-stargate--active-session nil)
  (setq dragonruby-stargate--session-index nil)
  (message "🌙 Stargate: Session finalized and persisted."))

(add-hook 'dragonruby-stargate-bridge-event-hook #'dragonruby-stargate-session-handle-event)

(provide 'dragonruby-stargate-manager)
