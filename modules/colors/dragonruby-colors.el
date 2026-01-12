;;; dragonruby-colors.el --- Smart Color Scanning (RGBA-aware, Non-Intrusive) -*- lexical-binding: t; -*-

(require 'dragonruby-kernel)
(require 'dragonruby-core)
(require 'dragonruby-color-utils)
(require 'dragonruby-color-visuals)
(require 'dragonruby-color-scanner)

;; 🧱 LEGACY MINOR MODES REMOVED

;; -----------------------------------------------------------------------------
;; 🧬 Lifecycle
;; -----------------------------------------------------------------------------

(defun dragonruby-colors-enable ()
  "Enable color visualization."
  (dragonruby-kernel-register-hook 'dragonruby-scan-hook #'dragonruby--scan-colors t)
  (message "🎨 Colors Module Enabled"))

(defun dragonruby-colors-disable ()
  "Disable color visualization."
  (remove-hook 'dragonruby-scan-hook #'dragonruby--scan-colors t)
  (dragonruby--clear-color-overlays)
  (message "🎨 Colors Module Disabled"))

;; -----------------------------------------------------------------------------
;; 📜 Manifest
;; -----------------------------------------------------------------------------

(dragonruby-register-module
 :name 'colors
 :type :main
 :namespace "dragonruby-color-"
 :provides '(:visuals :color-rendering)
 :requires nil
 :entry-point 'dragonruby-colors
 :enable-fn #'dragonruby-colors-enable
 :disable-fn #'dragonruby-colors-disable)

(provide 'dragonruby-colors)
;;; dragonruby-colors.el ends here
