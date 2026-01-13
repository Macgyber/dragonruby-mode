;;; manager.el --- Stargate Session Management and Asset Vault -*- lexical-binding: t -*-

;;; Commentary:
;; This module manages the persistence of .dr-stargate sessions.
;; It coordinates with the Ruby Runtime to capture state and saves it 
;; in a content-addressable Asset Vault.

(require 'json)
(require 'dragonruby-utils)

(defvar dragonruby-stargate--active-session nil
  "Path to the currently active .dr-stargate session directory.")

(defun dragonruby-stargate-session-init (project-root)
  "Initialize a new Stargate session in PROJECT-ROOT."
  (let* ((stargate-dir (expand-file-name ".stargate/" project-root))
         (session-dir (expand-file-name (format-time-string "session-%Y%m%d-%H%M%S/") stargate-dir))
         (blobs-dir (expand-file-name "blobs/" session-dir)))
    (unless (file-exists-p blobs-dir)
      (make-directory blobs-dir t))
    (setq dragonruby-stargate--active-session session-dir)
    (dragonruby-stargate-session-save-index)
    (message "🌌 Stargate: Session initialized at %s" session-dir)))

(defun dragonruby-stargate-session-save-index ()
  "Save the current session index (branches and moments metadata)."
  (if dragonruby-stargate--active-session
      (let ((index-file (expand-file-name "session.json" dragonruby-stargate--active-session))
            (data (list (cons "version" "1.0.0")
                        ;; Metadata for UI only, generated outside the runtime tick.
                        (cons "metadata" (list (cons "observed_at" (float-time))))
                        (cons "branches" '())
                        (cons "moments" '()))))
        (with-temp-file index-file
          (let ((json-encoding-object-type 'alist))
            (insert (json-encode data)))))
    (error "No active Stargate session")))

(defun dragonruby-stargate-vault-store (hash data)
  "Store binary or string DATA in the Asset Vault indexed by HASH."
  (if dragonruby-stargate--active-session
      (let ((blob-path (expand-file-name hash (expand-file-name "blobs/" dragonruby-stargate--active-session))))
        (unless (file-exists-p blob-path)
          (with-temp-file blob-path
            (insert data))))
    (error "No active Stargate session")))

(defun dragonruby-stargate-vault-get (hash)
  "Retrieve data from the Asset Vault via HASH."
  (if dragonruby-stargate--active-session
      (let ((blob-path (expand-file-name hash (expand-file-name "blobs/" dragonruby-stargate--active-session))))
        (if (file-exists-p blob-path)
            (with-temp-buffer
              (insert-file-contents blob-path)
              (buffer-string))
          (nil)))
    (error "No active Stargate session")))

(defun dragonruby-stargate-session-handle-event (event)
  "Dispatch Stargate EVENT from the bridge to the appropriate recorder."
  (let ((type (cdr (assoc 'type event))))
    (message "📝 Stargate Chronicler: Dispatching event type: %s" type)
    (cond
     ((string= type "moment") (dragonruby-stargate-session-record-moment event))
     ((string= type "branch") (dragonruby-stargate-session-record-branch event)))))

(defun dragonruby-stargate-session-record-moment (event)
  "Record an incoming Stargate moment from EVENT.
Stores the state in the vault and updates the session index."
  (let* ((address (cdr (assoc 'address event))) ;; branch@frame
         (hash (cdr (assoc 'hash event)))
         (seed (cdr (assoc 'seed event)))
         (data (cdr (assoc 'data event)))
         (observed-at (cdr (assoc 'observed_at event))))
    
    ;; 1. Store in Vault
    (dragonruby-stargate-vault-store hash data)
    
    ;; 2. Update Index
    (if dragonruby-stargate--active-session
        (let* ((index-file (expand-file-name "session.json" dragonruby-stargate--active-session))
               (json-object-type 'alist)
               (json-key-type 'string)
               (index (json-read-file index-file))
               (moments (cdr (assoc "moments" index))))
          
          (setf (alist-get address moments nil nil #'string=)
                `((hash . ,hash)
                  (seed . ,seed)
                  (observed_at . ,observed-at)))
          
          (setcdr (assoc "moments" index) moments)
          
          (with-temp-file index-file
            (let ((json-encoding-object-type 'alist))
              (insert (json-encode index))))
          (message "📝 Stargate Chronicler: Recorded moment %s" address))
      (error "No active session to record moment"))))

(defun dragonruby-stargate-session-record-branch (event)
  "Record an incoming Stargate branch from EVENT."
  (let* ((id (cdr (assoc 'id event)))
         (parent (cdr (assoc 'parent event)))
         (divergence (cdr (assoc 'divergence event))))
    
    (if dragonruby-stargate--active-session
        (let* ((index-file (expand-file-name "session.json" dragonruby-stargate--active-session))
               (json-object-type 'alist)
               (json-key-type 'string)
               (index (json-read-file index-file))
               (branches (cdr (assoc "branches" index))))
          
          (setf (alist-get id branches nil nil #'string=)
                `((parent . ,parent)
                  (divergence . ,divergence)))
          
          (setcdr (assoc "branches" index) branches)
          
          (with-temp-file index-file
            (let ((json-encoding-object-type 'alist))
              (insert (json-encode index))))
          (message "🌱 Stargate Chronicler: Recorded branch %s (parent: %s)" id parent))
      (error "No active session to record branch"))))

(add-hook 'dragonruby-stargate-bridge-event-hook #'dragonruby-stargate-session-handle-event)

(provide 'dragonruby-stargate-manager)
;;; manager.el ends here
