;;; load-plugin.el --- Dev loader for dragonruby-mode

(let ((root-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "src" root-dir))
  (add-to-list 'load-path (expand-file-name "src/core" root-dir))
  (add-to-list 'load-path (expand-file-name "src/ui" root-dir))
  (add-to-list 'load-path (expand-file-name "src/mode" root-dir))
  (add-to-list 'load-path (expand-file-name "src/concepts" root-dir)))

(require 'dragonruby)

(message "🐉 DragonRuby Mode loaded correctly from %s" default-directory)
