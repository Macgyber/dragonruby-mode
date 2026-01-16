;;; dragonruby-concepts--impl.el --- Concept Recognition implementation -*- lexical-binding: t; -*-

(require 'dragonruby-kernel)
(require 'dragonruby-core)
(require 'dragonruby-concept-visuals)

;;;###autoload
(defun dragonruby-concepts-enable ()
  "Enable concept highlighting."
  (dragonruby-kernel-register-hook 'dragonruby-scan-hook #'dragonruby--scan-concepts t)
  (message "🧠 Concepts Module Enabled"))

;;;###autoload
(defun dragonruby-concepts-disable ()
  "Disable concept highlighting."
  (remove-hook 'dragonruby-scan-hook #'dragonruby--scan-concepts t)
  (dragonruby--clear-concept-overlays)
  (message "🧠 Concepts Module Disabled"))

(provide 'dragonruby-concepts--impl)
;;; dragonruby-concepts--impl.el ends here
