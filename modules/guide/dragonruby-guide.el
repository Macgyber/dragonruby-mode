;;; dragonruby-guide.el --- Documentation linkage and lookup manifest -*- lexical-binding: t; -*-

(require 'dragonruby-kernel)

;; Autoload implementation functions
(autoload 'dragonruby-guide-enable "dragonruby-guide--impl")
(autoload 'dragonruby-guide-disable "dragonruby-guide--impl")
(autoload 'dragonruby-guide-toggle-sidebar "dragonruby-guide--impl" nil t)
(autoload 'dragonruby-guide-open-concept "dragonruby-guide--impl" nil t)
(autoload 'dragonruby-guide-open-concept-sidebar "dragonruby-guide--impl" nil t)

(defcustom dragonruby-experimental-concepts-guide nil
  "Enable experimental concept guidance."
  :type 'boolean
  :group 'dragonruby)

(defcustom dragonruby-guide-sidebar-position 'right
  "Position of the guidance sidebar.
Can be 'left, 'right, 'top, or 'bottom."
  :type '(choice (const left) (const right) (const top) (const bottom))
  :group 'dragonruby)

(defcustom dragonruby-guide-sidebar-width 0.3
  "Width ratio of the sidebar (0.0 to 1.0).
Example: 0.3 means 30% of frame width."
  :type 'float
  :group 'dragonruby)

(defcustom dragonruby-guide-follow-mode t 
  "If non-nil, the sidebar automatically follows the concept under the cursor."
  :type 'boolean
  :group 'dragonruby)

;;;###autoload
(defun dragonruby-guide ()
  "Main entry point for DragonRuby Guide. Toggles the sidebar."
  (interactive)
  (dragonruby-guide-toggle-sidebar))

;; -----------------------------------------------------------------------------
;; 📜 Manifest
;; -----------------------------------------------------------------------------

(dragonruby-register-module
 :name 'guide
 :type :main
 :namespace "dragonruby-guide-"
 :provides '(:guidance)
 :requires nil
 :entry-point 'dragonruby-guide--impl
 :enable-fn #'dragonruby-guide-enable
 :disable-fn #'dragonruby-guide-disable)

(provide 'dragonruby-guide)
;;; dragonruby-guide.el ends here
