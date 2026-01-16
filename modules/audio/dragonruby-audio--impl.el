;;; dragonruby-audio--impl.el --- Audio services implementation -*- lexical-binding: t; -*-

(require 'dragonruby-kernel)
(require 'dragonruby-core)
(require 'dragonruby-audio-model)
(require 'dragonruby-audio-fs)
(require 'dragonruby-audio-overlay)

;;;###autoload
(defun dragonruby-audio-enable ()
  "Enable audio services."
  (dragonruby-kernel-register-hook 'dragonruby-scan-hook #'dragonruby--scan-audio-overlays t)
  (message "🔊 Audio Module Enabled"))

;;;###autoload
(defun dragonruby-audio-disable ()
  "Disable audio services."
  (remove-hook 'dragonruby-scan-hook #'dragonruby--scan-audio-overlays t)
  (dragonruby--clear-audio-overlays)
  (message "🔊 Audio Module Disabled"))

(provide 'dragonruby-audio--impl)
;;; dragonruby-audio--impl.el ends here
