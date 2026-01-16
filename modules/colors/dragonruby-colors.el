;;; dragonruby-colors.el --- Color services manifest for DragonRuby -*- lexical-binding: t; -*-

(require 'dragonruby-kernel)

;; Autoload implementation functions
(autoload 'dragonruby-colors-enable "dragonruby-colors--impl")
(autoload 'dragonruby-colors-disable "dragonruby-colors--impl")

;; -----------------------------------------------------------------------------
;; 📜 Manifest
;; -----------------------------------------------------------------------------

(dragonruby-register-module
 :name 'colors
 :type :main
 :namespace "dragonruby-color-"
 :provides '(:visuals :color-rendering)
 :requires nil
 :entry-point 'dragonruby-colors--impl
 :enable-fn #'dragonruby-colors-enable
 :disable-fn #'dragonruby-colors-disable)

(provide 'dragonruby-colors)
;;; dragonruby-colors.el ends here
