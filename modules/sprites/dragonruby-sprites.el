;;; dragonruby-sprites.el --- Sprite services manifest for DragonRuby -*- lexical-binding: t; -*-

(require 'dragonruby-kernel)

;; Autoload implementation functions
(autoload 'dragonruby-sprites-enable "dragonruby-sprites--impl")
(autoload 'dragonruby-sprites-disable "dragonruby-sprites--impl")

;; -----------------------------------------------------------------------------
;; 📜 Manifest
;; -----------------------------------------------------------------------------

(dragonruby-register-module
 :name 'sprites
 :type :main
 :namespace "dragonruby-sprite-"
 :provides '(:visuals :sprite-detection :rendering)
 :requires nil
 :entry-point 'dragonruby-sprites--impl
 :enable-fn #'dragonruby-sprites-enable
 :disable-fn #'dragonruby-sprites-disable)

(provide 'dragonruby-sprites)
;;; dragonruby-sprites.el ends here
