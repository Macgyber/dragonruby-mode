;;; dragonruby-paths.el --- Universal Code & Data Navigation manifest -*- lexical-binding: t; -*-

(require 'dragonruby-kernel)

;; Autoload implementation functions
(autoload 'dragonruby-paths-enable "dragonruby-paths--impl")
(autoload 'dragonruby-paths-disable "dragonruby-paths--impl")

;; -----------------------------------------------------------------------------
;; 📜 Manifest
;; -----------------------------------------------------------------------------

(dragonruby-register-module
 :name 'paths
 :type :main
 :namespace "dragonruby-path" ;; Loose match for dragonruby-paths-mode/map too
 :provides '(:navigation :code-intelligence)
 :requires nil
 :entry-point 'dragonruby-paths--impl
 :enable-fn #'dragonruby-paths-enable
 :disable-fn #'dragonruby-paths-disable)

(provide 'dragonruby-paths)
;;; dragonruby-paths.el ends here
