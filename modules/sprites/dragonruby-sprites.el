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
  (add-hook 'dragonruby-scan-hook #'dragonruby--scan-sprites nil t)
  (add-hook 'dragonruby-monitor-hook #'dragonruby--sprite-popup-monitor-sync nil t)
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
