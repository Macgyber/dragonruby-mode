;;; dragonruby-knowledge.el --- Unified Knowledge Cache -*- lexical-binding: t; -*-

;; This is the "Single Source of Truth" cache.
;; It is filled by Guide (.org files) and consumed by Concepts (Retina).

(defvar dragonruby-knowledge-debug nil
  "Enable debug messages for the knowledge system.")

(defvar dragonruby--knowledge-cache (make-hash-table :test 'equal)
  "Global knowledge cache built from definitions (.org files).")

(defun dragonruby--knowledge-normalize (id)
  "Normalize ID to a clean lowercase string key.
Ensures :sprite, 'sprite, and \"sprite\" all map to \"sprite\"."
  (let* ((raw (format "%s" id))
         (norm (string-trim-left raw ":")))
    (downcase (substring-no-properties norm))))

(defun dragonruby-knowledge-clear ()
  "Wipe the knowledge cache.
Ensures zero stale data survives a reload or project change."
  (clrhash dragonruby--knowledge-cache)
  (when dragonruby-knowledge-debug
    (message "🧠 [Knowledge] Brain Wiped.")))

(defun dragonruby-knowledge-put (id data)
  "Store knowledge DATA for ID in the cache.
ID can be a string, symbol, or keyword.
DATA MUST be a plist with a string :desc."
  (unless (and (listp data) (stringp (plist-get data :desc)))
    (error "🧠 [Knowledge] DATA must be a plist with a string :desc"))
  (puthash (dragonruby--knowledge-normalize id) 
           data 
           dragonruby--knowledge-cache))

(defun dragonruby-knowledge-get (id)
  "Retrieve knowledge for ID from the cache.
Matches strings, symbols, or keywords regardless of case."
  (gethash (dragonruby--knowledge-normalize id) 
           dragonruby--knowledge-cache))

(defun dragonruby-knowledge-all-ids ()
  "Return all concept IDs currently in the cache."
  (hash-table-keys dragonruby--knowledge-cache))

(defun dragonruby-knowledge-inspect ()
  "Print the current state of the knowledge brain to Messages."
  (interactive)
  (let ((count (hash-table-count dragonruby--knowledge-cache)))
    (message "🧠 [Knowledge Audit] %d definitions in brain:" count)
    (maphash (lambda (k v)
               (let* ((desc (plist-get v :desc))
                      (s (if (stringp desc) desc "<invalid desc>")))
                 (message "  - %s: %s..." k 
                          (substring s 0 (min 50 (length s))))))
             dragonruby--knowledge-cache)))

(provide 'dragonruby-knowledge)
