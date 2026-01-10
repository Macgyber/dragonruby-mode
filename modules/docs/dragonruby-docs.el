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

(defun dragonruby-docs-enable ()
  "Enable documentation services."
  (message "📖 Documentation Module Enabled"))

(defun dragonruby-docs-disable ()
  "Disable documentation services."
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
