;;; dragonruby-sprites.el --- Sprite services for DragonRuby -*- lexical-binding: t; -*-

(require 'dragonruby-kernel)
(require 'dragonruby-core)
(require 'dragonruby-sprite-model)
(require 'dragonruby-sprite-fs)
(require 'dragonruby-sprite-overlay)
(require 'dragonruby-sprite-popup)
(require 'dragonruby-sprite-actions)

;; -----------------------------------------------------------------------------
;; 🧬 Lifecycle
;; -----------------------------------------------------------------------------

(defun dragonruby-sprites-enable ()
  "Enable sprite services."
  (dragonruby-kernel-register-hook 'dragonruby-scan-hook #'dragonruby--scan-sprites t)
  (dragonruby-kernel-register-hook 'dragonruby-monitor-hook #'dragonruby--sprite-popup-monitor-sync t)
  (message "🖼️ [Sprites] Module Enabled"))

(defun dragonruby-sprites-disable ()
  "Disable sprite services."
  (remove-hook 'dragonruby-scan-hook #'dragonruby--scan-sprites t)
  (remove-hook 'dragonruby-monitor-hook #'dragonruby--sprite-popup-monitor-sync t)
  (dragonruby--sprite-popup-cleanup)
  (dragonruby--clear-sprite-overlays)
  (message "🖼️ [Sprites] Module Disabled"))

;; -----------------------------------------------------------------------------
;; 📜 Manifest
;; -----------------------------------------------------------------------------

(dragonruby-register-module
 :name 'sprites
 :type :main
 :namespace "dragonruby-sprite-"
 :provides '(:visuals :sprite-detection :rendering)
 :requires nil
 :entry-point 'dragonruby-sprites
 :enable-fn #'dragonruby-sprites-enable
 :disable-fn #'dragonruby-sprites-disable)

(provide 'dragonruby-sprites)
;;; dragonruby-sprites.el ends here
