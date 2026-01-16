;;; dragonruby-colors--impl.el --- Smart Color Scanning implementation -*- lexical-binding: t; -*-

(require 'dragonruby-kernel)
(require 'dragonruby-core)
(require 'dragonruby-color-utils)
(require 'dragonruby-color-visuals)
(require 'dragonruby-color-scanner)

;;;###autoload
(defun dragonruby-colors-enable ()
  "Enable color visualization."
  (dragonruby-kernel-register-hook 'dragonruby-scan-hook #'dragonruby--scan-colors t)
  (message "🎨 Colors Module Enabled"))

;;;###autoload
(defun dragonruby-colors-disable ()
  "Disable color visualization."
  (remove-hook 'dragonruby-scan-hook #'dragonruby--scan-colors t)
  (dragonruby--clear-color-overlays)
  (message "🎨 Colors Module Disabled"))

(provide 'dragonruby-colors--impl)
;;; dragonruby-colors--impl.el ends here
