;;; dragonruby-path-actions.el --- Interactive path actions -*- lexical-binding: t; -*-

(require 'dragonruby-core)
(require 'dragonruby-path-fs)
(require 'dragonruby-path-snippets)
(require 'dragonruby-path-overlay)

(defun dragonruby-smart-complete ()
  "Smart completion for paths.
1. Try to expand a snippet (req, reqr, etc.)
2. If inside a string, trigger the completion-at-point system."
  (interactive)
  (unless (dragonruby--try-expand-snippet)
    (if (dragonruby--inside-string-p)
        (completion-at-point)
      (message "💡 Escribe 'req' y presiona C-M-i para insertar require"))))

(defun dragonruby-open-project-path ()
  "Open any supported project file using the minibuffer."
  (interactive)
  (let* ((root (dragonruby--find-project-root))
         (candidates (dragonruby--collect-project-files))
         (choice (completing-read "📁 DragonRuby open: " candidates nil t)))
    (when (and choice (not (string-empty-p choice)))
      (find-file (expand-file-name choice root)))))

(provide 'dragonruby-path-actions)
;;; dragonruby-path-actions.el ends here
