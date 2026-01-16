;;; dragonruby-font-tools.el --- Font tools manifest for DragonRuby -*- lexical-binding: t; -*-

(require 'dragonruby-kernel)

;; Autoload implementation functions
(autoload 'dragonruby-font-tools-enable "dragonruby-font-tools--impl")
(autoload 'dragonruby-font-tools-disable "dragonruby-font-tools--impl")

;; -----------------------------------------------------------------------------
;; 📜 Manifest
;; -----------------------------------------------------------------------------

(dragonruby-register-module
 :name 'font-tools
 :type :tool
 :namespace "dragonruby-font-" ;; Shares namespace with fonts for now
 :provides '(:font-manipulation)
 :requires '(:font-rendering)
 :entry-point 'dragonruby-font-tools--impl
 :enable-fn #'dragonruby-font-tools-enable
 :disable-fn #'dragonruby-font-tools-disable)

(provide 'dragonruby-font-tools)
;;; dragonruby-font-tools.el ends here
