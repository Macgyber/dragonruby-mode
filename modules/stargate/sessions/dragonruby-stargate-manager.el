;;; manager.el --- Stargate Session Management and Asset Vault -*- lexical-binding: t -*-

(require 'json)
(require 'dragonruby-utils)
(require 'cl-lib)

(defvar dragonruby-stargate--active-session nil
  "Path to the currently active .dr-stargate session directory.")

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
    (dragonruby-stargate-session-save-index)
    (message "🌌 Stargate: Session initialized at %s" session-dir)))

(defun dragonruby-stargate-session-save-index ()
  "Save the current session index (branches and moments metadata)."
  (if dragonruby-stargate--active-session
      (let ((index-file (expand-file-name "session.json" dragonruby-stargate--active-session))
            (data (list (cons "version" "1.0.0")
                        (cons "metadata" (list (cons "observed_at" (float-time))))
                        (cons "branches" nil)
                        (cons "moments" nil))))
        (with-temp-file index-file
          (let ((json-encoding-object-type 'alist))
            (insert (json-encode data)))))
    (error "No active Stargate session")))

(defun dragonruby-stargate-vault-store (hash data)
  "Store string DATA in the Asset Vault indexed by HASH."
  (when (and dragonruby-stargate--active-session data hash (not (string= hash "000000")))
    (let ((blob-path (expand-file-name hash (expand-file-name "blobs/" dragonruby-stargate--active-session))))
      (unless (file-exists-p blob-path)
        (with-temp-file blob-path
          (insert data))))))

(defun dragonruby-stargate-vault-get (hash)
  "Retrieve string DATA from the Asset Vault indexed by HASH."
  (if (and dragonruby-stargate--active-session hash)
      (let ((blob-path (expand-file-name hash (expand-file-name "blobs/" dragonruby-stargate--active-session))))
        (if (file-exists-p blob-path)
            (with-temp-buffer
              (insert-file-contents blob-path)
              (buffer-string))
          (progn
            (message "⚠️ Stargate Vault: Blob %s not found in session." hash)
            nil)))
    (progn
      (message "⚠️ Stargate Vault: No active session or hash.")
      nil)))

(defun dragonruby-stargate-session-handle-event (event)
  "Dispatch Stargate EVENT from the bridge to the appropriate recorder."
  (let ((type (cdr (assoc "type" event))))
    (cond
     ((string= type "moment") (dragonruby-stargate-session-record-moment event))
     ((string= type "branch") (dragonruby-stargate-session-record-branch event)))))

(defun dragonruby-stargate-session-record-moment (event)
  "Record an incoming Stargate moment from EVENT."
  (let* ((address (cdr (assoc "address" event)))
         (hash (cdr (assoc "hash" event)))
         (seed (cdr (assoc "seed" event)))
         (data (cdr (assoc "data" event)))
         (moment-type (cdr (assoc "moment_type" event)))
         (observed-at (cdr (assoc "observed_at" event))))
    
    (dragonruby-stargate-vault-store hash data)
    
    (when dragonruby-stargate--active-session
      (let* ((index-file (expand-file-name "session.json" dragonruby-stargate--active-session))
             (json-object-type 'alist)
             (json-key-type 'string)
             (json-array-type 'list)
             (index (condition-case err 
                        (json-read-file index-file)
                      (error 
                       (message "❌ Stargate: Error reading index-file: %s" err)
                       nil))))
        
        (when index
          (let ((moments (let ((m (cdr (assoc "moments" index))))
                           (if (vectorp m) (append m nil) m))))
            
            (message "💾 Stargate: Recording %s (Hash: %s) to %s" address hash index-file)
            
            ;; Ensure moments is a list for alist-get/remove
            (setq moments (append moments nil))
            (setq moments (cons (cons address `((hash . ,hash)
                                               (seed . ,seed)
                                               (moment_type . ,moment-type)
                                               (observed_at . ,observed-at)))
                                (cl-remove address moments :key #'car :test #'string=)))
            
            (setcdr (assoc "moments" index) moments)
            
            (with-temp-file index-file
              (let ((json-encoding-object-type 'alist))
                (insert (json-encode index))))))))))

(defun dragonruby-stargate-session-record-branch (event)
  "Record an incoming Stargate branch from EVENT."
  (let* ((id (cdr (assoc "id" event)))
         (parent (cdr (assoc "parent" event)))
         (divergence (cdr (assoc "divergence" event))))
    
    (when dragonruby-stargate--active-session
      (let* ((index-file (expand-file-name "session.json" dragonruby-stargate--active-session))
             (json-object-type 'alist)
             (json-key-type 'string)
             (json-array-type 'list)
             (index (json-read-file index-file))
             (branches (let ((b (cdr (assoc "branches" index))))
                         (if (vectorp b) (append b nil) b))))
        
        (setq branches (append branches nil))
        (setq branches (cons (cons id `((parent . ,parent) (divergence . ,divergence)))
                             (cl-remove id branches :key #'car :test #'string=)))
        
        (setcdr (assoc "branches" index) branches)
        
        (with-temp-file index-file
          (let ((json-encoding-object-type 'alist))
            (insert (json-encode index))))))))

(add-hook 'dragonruby-stargate-bridge-event-hook #'dragonruby-stargate-session-handle-event)

(provide 'dragonruby-stargate-manager)
