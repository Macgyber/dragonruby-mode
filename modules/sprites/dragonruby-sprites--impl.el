;;; dragonruby-sprites--impl.el --- Sprite services implementation -*- lexical-binding: t; -*-

(require 'dragonruby-kernel)
(require 'dragonruby-core)
(require 'dragonruby-sprite-model)
(require 'dragonruby-sprite-fs)
(require 'dragonruby-sprite-overlay)
(require 'dragonruby-sprite-popup)
(require 'dragonruby-sprite-actions)

;;;###autoload
(defun dragonruby-sprites-enable ()
  "Enable sprite services."
  (dragonruby-kernel-register-hook 'dragonruby-scan-hook #'dragonruby--scan-sprites t)
  (dragonruby-kernel-register-hook 'dragonruby-monitor-hook #'dragonruby--sprite-popup-monitor-sync t)
  (message "🖼️ [Sprites] Module Enabled"))

;;;###autoload
(defun dragonruby-sprites-disable ()
  "Disable sprite services."
  (remove-hook 'dragonruby-scan-hook #'dragonruby--scan-sprites t)
  (remove-hook 'dragonruby-monitor-hook #'dragonruby--sprite-popup-monitor-sync t)
  (dragonruby--sprite-popup-cleanup)
  (dragonruby--clear-sprite-overlays)
  (message "🖼️ [Sprites] Module Disabled"))

(provide 'dragonruby-sprites--impl)
;;; dragonruby-sprites--impl.el ends here
