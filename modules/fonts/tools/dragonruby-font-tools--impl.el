;;; dragonruby-font-tools--impl.el --- Font Tools implementation -*- lexical-binding: t; -*-

(require 'dragonruby-kernel)
(require 'dragonruby-font-engine)
(require 'dragonruby-font-ui)

;;;###autoload
(defun dragonruby-font-tools-enable ()
  "Enable font tools."
  (message "🛠️ Font Tools Enabled"))

;;;###autoload
(defun dragonruby-font-tools-disable ()
  "Disable font tools."
  (message "🛠️ Font Tools Disabled"))

(provide 'dragonruby-font-tools--impl)
;;; dragonruby-font-tools--impl.el ends here
