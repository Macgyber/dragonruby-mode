;;; dragonruby-fonts.el --- Font services manifest for DragonRuby -*- lexical-binding: t; -*-

(require 'dragonruby-kernel)

;; Autoload implementation functions
(autoload 'dragonruby-fonts-enable "dragonruby-fonts--impl")
(autoload 'dragonruby-fonts-disable "dragonruby-fonts--impl")
(autoload 'dragonruby-font-viewer-mode "dragonruby-fonts--impl" nil t)

;; Register font viewer for TTF/OTF files
(add-to-list 'auto-mode-alist '("\\.\\(ttf\\|otf\\)\\'" . dragonruby-font-viewer-mode))

;; -----------------------------------------------------------------------------
;; 📜 Manifest
;; -----------------------------------------------------------------------------

(dragonruby-register-module
 :name 'fonts
 :type :main
 :namespace "dragonruby-font-"
 :provides '(:visuals :font-rendering)
 :requires nil
 :entry-point 'dragonruby-fonts--impl
 :enable-fn #'dragonruby-fonts-enable
 :disable-fn #'dragonruby-fonts-disable)

(provide 'dragonruby-fonts)
;;; dragonruby-fonts.el ends here
