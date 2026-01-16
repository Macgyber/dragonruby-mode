;;; dragonruby-fonts--impl.el --- Font services and viewer implementation -*- lexical-binding: t; -*-

(require 'dragonruby-kernel)
(require 'dragonruby-core)
(require 'dragonruby-font-model)
(require 'dragonruby-font-fs)
(require 'dragonruby-font-overlay)
(require 'dragonruby-font-completion)
(require 'dragonruby-font-ui)

;;;###autoload
(define-derived-mode dragonruby-font-viewer-mode special-mode "DR-Font"
  "Major mode for viewing font files in DragonRuby."
  (setq cursor-type nil)
  (dragonruby--setup-font-header-line)
  (dragonruby-font-viewer-refresh))

;;;###autoload
(defun dragonruby-fonts-enable ()
  "Enable font services."
  (dragonruby-kernel-register-hook 'dragonruby-scan-hook #'dragonruby--scan-font-overlays t)
  (dragonruby--setup-font-capf)
  (message "🅰️ Fonts Module Enabled"))

;;;###autoload
(defun dragonruby-fonts-disable ()
  "Disable font services."
  (remove-hook 'dragonruby-scan-hook #'dragonruby--scan-font-overlays t)
  (dragonruby--clear-font-overlays)
  (message "🅰️ Fonts Module Disabled"))

(provide 'dragonruby-fonts--impl)
;;; dragonruby-fonts--impl.el ends here
