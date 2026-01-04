;;; dragonruby-font-fs.el --- Filesystem operations for fonts -*- lexical-binding: t; -*-

(require 'dragonruby-core)
(require 'dragonruby-assets)

(defun dragonruby--get-all-fonts-in-project ()
  "Get all font files in the project using the centralized core engine."
  (dragonruby--collect-project-files 'font))

(provide 'dragonruby-font-fs)
;;; dragonruby-font-fs.el ends here
