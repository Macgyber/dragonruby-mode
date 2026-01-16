;;; dragonruby-concepts.el --- Concept recognition manifest for DragonRuby -*- lexical-binding: t; -*-

(require 'dragonruby-kernel)

;; Autoload implementation functions
(autoload 'dragonruby-concepts-enable "dragonruby-concepts--impl")
(autoload 'dragonruby-concepts-disable "dragonruby-concepts--impl")

;; -----------------------------------------------------------------------------
;; 📜 Manifest
;; -----------------------------------------------------------------------------

(dragonruby-register-module
 :name 'concepts
 :type :main
 :namespace "dragonruby-concept-"
 :provides '(:visuals :code-education)
 :requires '(:guidance)
 :entry-point 'dragonruby-concepts--impl
 :enable-fn #'dragonruby-concepts-enable
 :disable-fn #'dragonruby-concepts-disable)

(provide 'dragonruby-concepts)
;;; dragonruby-concepts.el ends here
