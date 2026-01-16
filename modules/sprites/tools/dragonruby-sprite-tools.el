;;; dragonruby-sprite-tools.el --- Sprite tools manifest for DragonRuby -*- lexical-binding: t; -*-

(require 'dragonruby-kernel)

;; Autoload implementation functions
(autoload 'dragonruby-sprite-tools-enable "dragonruby-sprite-tools--impl")
(autoload 'dragonruby-sprite-tools-disable "dragonruby-sprite-tools--impl")

;; -----------------------------------------------------------------------------
;; 📜 Manifest
;; -----------------------------------------------------------------------------

(dragonruby-register-module
 :name 'sprite-tools
 :type :tool
 :namespace "dragonruby-image-" ;; Legacy prefix retention for now
 :provides '(:image-manipulation)
 :requires '(:rendering) ;; Provided by 'sprites
 :entry-point 'dragonruby-sprite-tools--impl
 :enable-fn #'dragonruby-sprite-tools-enable
 :disable-fn #'dragonruby-sprite-tools-disable)

(provide 'dragonruby-sprite-tools)
;;; dragonruby-sprite-tools.el ends here
