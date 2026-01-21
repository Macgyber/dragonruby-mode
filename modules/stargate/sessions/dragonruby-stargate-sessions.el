;;; dragonruby-stargate-sessions.el --- Stargate Session Data Persistence -*- lexical-binding: t -*-

(require 'json)
(require 'dragonruby-utils)

(defvar dragonruby-stargate--active-session nil "Path to the active session directory.")
(defvar dragonruby-stargate--session-index nil "The master JSON index of the current session.")

(defun dragonruby-stargate-session-init (&optional project-root)
  "Create a new physical session on disk."
  (let* ((root (or project-root (dragonruby--find-project-root)))
         (session-id (format-time-string "session-%Y%m%d-%H%M%S"))
         (session-dir (expand-file-name (concat ".stargate/" session-id "/") root))
         (blobs-dir (expand-file-name "blobs/" session-dir)))
    (unless (file-exists-p blobs-dir) (make-directory blobs-dir t))
    (setq dragonruby-stargate--active-session session-dir)
    (setq dragonruby-stargate--session-index (dragonruby-stargate-session--new-index session-id))
    (dragonruby-stargate-session-persist t)
    (message "🌌 Stargate: Session %s anchored to disk." session-id)))

(defun dragonruby-stargate-session--new-index (id)
  "Generate a fresh session data structure."
  (let ((branches (make-hash-table :test 'equal))
        (moments (make-hash-table :test 'equal))
        (maps (make-hash-table :test 'equal)))
    (puthash "prime" (list (cons "parent" nil) (cons "divergence" 0)) branches)
    (puthash "prime" '() maps)
    (list (cons "session_id" id) (cons "branches" branches) 
          (cons "moments" moments) (cons "branch-maps" maps))))

(defun dragonruby-stargate-session-persist (&optional _force)
  "Flush current index to disk."
  (when (and dragonruby-stargate--active-session dragonruby-stargate--session-index)
    (let ((index-file (expand-file-name "index.json" dragonruby-stargate--active-session)))
      (with-temp-file index-file (insert (json-encode dragonruby-stargate--session-index))))))

(defun dragonruby-stargate-session-load (session-dir)
  "Load an existing session from disk."
  (interactive "DSession Directory: ")
  (let ((index-file (expand-file-name "index.json" session-dir)))
    (unless (file-exists-p index-file) (error "Stargate: No index.json in %s" session-dir))
    (setq dragonruby-stargate--active-session session-dir)
    (with-temp-buffer
      (insert-file-contents index-file)
      (setq dragonruby-stargate--session-index (json-read)))
    (message "🌌 Stargate: Session %s loaded." (file-name-nondirectory (directory-file-name session-dir)))))

(defun dragonruby-stargate-session-stop ()
  "Gracefully shutdown and persist the current session."
  (when dragonruby-stargate--active-session
    (dragonruby-stargate-session-persist t)
    (setq dragonruby-stargate--active-session nil
          dragonruby-stargate--session-index nil)
    (message "🌌 Stargate: Session archived.")))

(provide 'dragonruby-stargate-sessions)
