;;; dragonruby-completion.el --- API Contract completion manifest for DragonRuby -*- lexical-binding: t; -*-

(require 'dragonruby-kernel)

;; Autoload implementation functions
(autoload 'dragonruby-completion-enable "dragonruby-completion--impl")
(autoload 'dragonruby-completion-disable "dragonruby-completion--impl")
(autoload 'dragonruby-completion-self-insert-dot "dragonruby-completion--impl" nil t)

;; -----------------------------------------------------------------------------
;; 📜 Manifest
;; -----------------------------------------------------------------------------

(dragonruby-register-module
 :name 'completion
 :type :main
 :namespace "dragonruby-completion-"
 :provides '(:completion)
 :requires nil
 :entry-point 'dragonruby-completion--impl
 :enable-fn #'dragonruby-completion-enable
 :disable-fn #'dragonruby-completion-disable)

(provide 'dragonruby-completion)
;;; dragonruby-completion.el ends here
