;;; dragonruby-sprite-fs.el --- Filesystem operations for sprites -*- lexical-binding: t; -*-

(require 'dragonruby-core)

(defun dragonruby--resolve-asset-path (path)
  "Resolve PATH relative to project root."
  (let ((root (dragonruby--find-project-root)))
    (when (and path root)
      (expand-file-name path root))))

(defun dragonruby--get-all-sprites-in-project ()
  "Get all sprite files in the project."
  (let ((root (dragonruby--find-project-root)))
    (when (and root (file-directory-p root))
      (let ((files (directory-files-recursively root "\\.\\(png\\|jpg\\|jpeg\\|gif\\|bmp\\|PNG\\|JPG\\)$")))
        (mapcar (lambda (f) (file-relative-name f root)) files)))))

(provide 'dragonruby-sprite-fs)
;;; dragonruby-sprite-fs.el ends here
