;;; dragonruby-concepts.el --- Concept Recognition Facade -*- lexical-binding: t; -*-

(require 'dragonruby-kernel)
(require 'dragonruby-core)
(require 'dragonruby-concept-visuals)

;; 🧱 LEGACY MINOR MODES REMOVED

;; -----------------------------------------------------------------------------
;; 🧬 Lifecycle
;; -----------------------------------------------------------------------------

(defun dragonruby-concepts-enable ()
  "Enable concept highlighting."
  (dragonruby-kernel-register-hook 'dragonruby-scan-hook #'dragonruby--scan-concepts t)
  (message "🧠 Concepts Module Enabled"))

(defun dragonruby-concepts-disable ()
  "Disable concept highlighting."
  (remove-hook 'dragonruby-scan-hook #'dragonruby--scan-concepts t)
  (dragonruby--clear-concept-overlays)
  (message "🧠 Concepts Module Disabled"))

;; -----------------------------------------------------------------------------
;; 📜 Manifest
;; -----------------------------------------------------------------------------

(dragonruby-register-module
 :name 'concepts
 :type :main
 :namespace "dragonruby-concept-"
 :provides '(:visuals :code-education)
 :requires '(:guidance)
 :entry-point 'dragonruby-concepts
 :enable-fn #'dragonruby-concepts-enable
 :disable-fn #'dragonruby-concepts-disable)

(provide 'dragonruby-concepts)
;;; dragonruby-concepts.el ends here
