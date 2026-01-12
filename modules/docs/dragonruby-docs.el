;;; dragonruby-docs.el --- Documentation linkage and lookup -*- lexical-binding: t; -*-

(require 'dragonruby-kernel)
(require 'dragonruby-core)
(require 'org)

(defvar dragonruby-docs-directory
  (let ((current-dir (file-name-directory (or load-file-name buffer-file-name))))
    ;; From modules/docs/ -> ../../docs/concepts
    (expand-file-name "../../docs/concepts" current-dir))
  "Directory containing local DragonRuby documentation concepts.")

(defvar dragonruby-docs--concept-map nil
  "Hash table mapping search terms to docs filenames.")

(defcustom dragonruby-experimental-concepts-docs nil
  "Enable experimental concept documentation."
  :type 'boolean
  :group 'dragonruby)

;; 🧱 LEGACY MINOR MODE REMOVED

;; -----------------------------------------------------------------------------
;; 🧬 Lifecycle
;; -----------------------------------------------------------------------------

(defun dragonruby-docs--build-concept-map ()
  "Build mapping from concept keywords to .org files.
Auto-discovers all .org files in the concepts directory."
  (let ((map (make-hash-table :test 'equal)))
    (when (file-directory-p dragonruby-docs-directory)
      (dolist (file (directory-files dragonruby-docs-directory nil "\\.org$"))
        (unless (string-prefix-p "_" file) ; Skip templates
          (let ((concept-name (file-name-sans-extension file)))
            (puthash concept-name file map)))))
    map))

(defun dragonruby-docs-open-concept (concept-name)
  "Open CONCEPT-NAME in its Org file with narrowing (Focused Learning).
Falls back to simple message if file not found."
  (interactive "sConcept: ")
  (unless dragonruby-docs--concept-map
    (setq dragonruby-docs--concept-map (dragonruby-docs--build-concept-map)))
  
  (let ((org-file (gethash concept-name dragonruby-docs--concept-map)))
    (if org-file
        (let ((full-path (expand-file-name org-file dragonruby-docs-directory)))
          (if (file-exists-p full-path)
              (progn
                ;; Open in a new window
                (find-file-other-window full-path)
                ;; Widen first (in case already narrowed)
                (widen)
                ;; Jump to the main heading
                (goto-char (point-min))
                (when (re-search-forward (format "^\\*+ %s" (regexp-quote concept-name)) nil t)
                  (org-show-subtree)
                  (org-narrow-to-subtree)
                  (goto-char (point-min))
                  (message "📖 Focused on: %s (C-x n w to widen)" concept-name)))
            (message "⚠️ Org file exists in map but not on disk: %s" full-path)))
      (message "ℹ️ No Org documentation for '%s'" concept-name))))

(defun dragonruby-docs-enable ()
  "Enable documentation services."
  ;; Initialize the concept map
  (setq dragonruby-docs--concept-map (dragonruby-docs--build-concept-map))
  (message "📖 Documentation Module Enabled (Concepts: %d)" 
           (hash-table-count dragonruby-docs--concept-map)))

(defun dragonruby-docs-disable ()
  "Disable documentation services."
  (setq dragonruby-docs--concept-map nil)
  (message "📖 Documentation Module Disabled"))

;; -----------------------------------------------------------------------------
;; 📜 Manifest
;; -----------------------------------------------------------------------------

(dragonruby-register-module
 :name 'docs
 :type :main
 :namespace "dragonruby-docs-"
 :provides '(:documentation)
 :requires nil
 :entry-point 'dragonruby-docs
 :enable-fn #'dragonruby-docs-enable
 :disable-fn #'dragonruby-docs-disable)

(provide 'dragonruby-docs)
;;; dragonruby-docs.el ends here
