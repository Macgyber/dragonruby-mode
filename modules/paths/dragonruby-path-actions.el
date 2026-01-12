;;; dragonruby-path-actions.el --- Interactive path actions -*- lexical-binding: t; -*-

(require 'dragonruby-core)
(require 'dragonruby-path-fs)
(require 'dragonruby-path-snippets)
(require 'dragonruby-path-overlay)
(require 'dragonruby-path-completion)

(defun dragonruby-smart-complete ()
  "Smart completion for DragonRuby paths (The Law).
Flow: req -> C-M-i (expand) -> C-M-i (Minibuffer List).

This command strictly follows the requested workflow:
1. Expands snippets if point is after a keyword (e.g., \\='req\\=').
2. Triggers a minibuffer file selector with formatted candidates."
  (interactive)
  ;; ESCUDO: Don't try to open a minibuffer if we are already in one
  (if (minibufferp)
      (minibuffer-complete)
    (unless (dragonruby--try-expand-snippet)
      (if (dragonruby--inside-string-p)
          (let ((context (dragonruby--path-context)))
            (if (not context)
                (dragonruby--notify 'path-context-hint "💡 Point is not inside a valid path" t)
              (let* ((start (nth 0 context))
                     (end (nth 1 context))
                     (type (nth 2 context))
                     (initial (buffer-substring-no-properties start end))
                     ;; THE LAW: Respect context type (ruby, data, or sprite)
                     (raw-candidates (dragonruby--collect-project-files type))
                     ;; Create visual mapping
                     (display-map (make-hash-table :test 'equal))
                     (display-candidates
                      (mapcar (lambda (path)
                                (let* ((ext (or (file-name-extension path) "??"))
                                       (display (format "%-40s (%s)" path (downcase ext))))
                                  (puthash display path display-map)
                                  display))
                              raw-candidates))
                     ;; ULTRA-MINIMALIST: Just the list.
                     (prompt "📁 Path: ")
                     ;; INSTANT LIST
                     (choice (minibuffer-with-setup-hook
                                 (lambda () (minibuffer-completion-help))
                               (completing-read prompt display-candidates nil t initial))))
                (when (and choice (gethash choice display-map))
                  (delete-region start end)
                  (insert (gethash choice display-map))))))
        ;; FALLBACK: If not a snippet and not in a path, use standard CAPF
        (completion-at-point)))))

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
