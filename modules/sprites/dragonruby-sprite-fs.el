;;; dragonruby-sprite-fs.el --- Filesystem operations for sprites -*- lexical-binding: t; -*-

(require 'dragonruby-core)

(defun dragonruby--resolve-asset-path (path)
  "Resolve PATH relative to project root."
  (let ((root (dragonruby--find-project-root)))
    (when (and path root)
      (expand-file-name path root))))

(defun dragonruby--get-all-sprites-in-project ()
  "Get all sprite files in the project using the centralized core engine."
  (dragonruby--collect-project-files 'sprite))

(provide 'dragonruby-sprite-fs)
;;; dragonruby-sprite-fs.el ends here
