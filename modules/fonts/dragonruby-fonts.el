;;; dragonruby-fonts.el --- Font services and viewer for DragonRuby -*- lexical-binding: t; -*-

(require 'dragonruby-kernel)
(require 'dragonruby-core)
(require 'dragonruby-font-model)
(require 'dragonruby-font-fs)
(require 'dragonruby-font-overlay)
(require 'dragonruby-font-completion)
(require 'dragonruby-font-ui)

;; 🧱 LEGACY MINOR MODES REMOVED
;; Use (dragonruby-enable 'fonts)

;; Font Viewer Major Mode (Specific file handler, can remain autoloaded but logically attached)
(define-derived-mode dragonruby-font-viewer-mode special-mode "DR-Font"
  "Major mode for viewing font files in DragonRuby."
  (setq cursor-type nil)
  (dragonruby--setup-font-header-line)
  (dragonruby-font-viewer-refresh))

;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(ttf\\|otf\\)\\'" . dragonruby-font-viewer-mode))

;; -----------------------------------------------------------------------------
;; 🧬 Lifecycle
;; -----------------------------------------------------------------------------

(defun dragonruby-fonts-enable ()
  "Enable font services."
  (dragonruby-kernel-register-hook 'dragonruby-scan-hook #'dragonruby--scan-font-overlays t)
  (dragonruby--setup-font-capf)
  (message "🅰️ Fonts Module Enabled"))

(defun dragonruby-fonts-disable ()
  "Disable font services."
  (remove-hook 'dragonruby-scan-hook #'dragonruby--scan-font-overlays t)
  (dragonruby--clear-font-overlays)
  (message "🅰️ Fonts Module Disabled"))

;; -----------------------------------------------------------------------------
;; 📜 Manifest
;; -----------------------------------------------------------------------------

(dragonruby-register-module
 :name 'fonts
 :type :main
 :namespace "dragonruby-font-"
 :provides '(:visuals :font-rendering)
 :requires nil
 :entry-point 'dragonruby-fonts
 :enable-fn #'dragonruby-fonts-enable
 :disable-fn #'dragonruby-fonts-disable)

(provide 'dragonruby-fonts)
;;; dragonruby-fonts.el ends here
