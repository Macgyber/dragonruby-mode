;;; dragonruby-path-fs.el --- Filesystem operations for paths -*- lexical-binding: t; -*-

(require 'dragonruby-core)
(require 'dragonruby-path-model)

(defun dragonruby--resolve-path (raw-path type)
  "Resolve RAW-PATH based on TYPE (\\='require, \\='require_relative, \\='load).
If TYPE is \\='require or \\=load, resolve relative to project root.
If TYPE is \\='require_relative, resolve relative to current file."
  (let* ((root (dragonruby--find-project-root))
         (base-dir (if (eq type 'require_relative)
                       (file-name-directory (or buffer-file-name default-directory))
                     root))
         (ensure-rb (memq type '(require require_relative)))
         (path (if (and ensure-rb (not (string-suffix-p ".rb" raw-path)))
                   (concat raw-path ".rb")
                 raw-path)))
    (when (and path base-dir)
      (expand-file-name path base-dir))))

(defun dragonruby--collect-project-files (&optional type)
  "Return project files relative to root.
TYPE can be \\='ruby, \\='data, or nil for all."
  (let* ((root (dragonruby--find-project-root))
         (ruby-exts dragonruby-ruby-extensions)
         (data-exts dragonruby-data-extensions)
         (extensions (pcase type
                       ('ruby ruby-exts)
                       ('data data-exts)
                       (_ (append ruby-exts data-exts))))
         (pattern (concat "\\.\\(" (regexp-opt extensions) "\\)$")))
    (when (and root (file-directory-p root))
      (let ((files (directory-files-recursively root pattern)))
        (mapcar (lambda (f) (file-relative-name f root)) files)))))

(provide 'dragonruby-path-fs)
;;; dragonruby-path-fs.el ends here
