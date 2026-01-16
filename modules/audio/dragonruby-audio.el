;;; dragonruby-audio.el --- Audio services manifest for DragonRuby -*- lexical-binding: t; -*-

(require 'dragonruby-kernel)

;; Autoload implementation functions
(autoload 'dragonruby-audio-enable "dragonruby-audio--impl")
(autoload 'dragonruby-audio-disable "dragonruby-audio--impl")

;; -----------------------------------------------------------------------------
;; 📜 Manifest
;; -----------------------------------------------------------------------------

(dragonruby-register-module
 :name 'audio
 :type :main
 :namespace "dragonruby-audio-"
 :provides '(:visuals :audio-detection)
 :requires nil
 :entry-point 'dragonruby-audio--impl
 :enable-fn #'dragonruby-audio-enable
 :disable-fn #'dragonruby-audio-disable)

(provide 'dragonruby-audio)
;;; dragonruby-audio.el ends here
