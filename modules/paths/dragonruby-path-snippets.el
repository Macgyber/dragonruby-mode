;;; dragonruby-path-snippets.el --- Snippet expansion -*- lexical-binding: t; -*-

(require 'dragonruby-core)

(require 'cl-lib)
(require 'dragonruby-path-model)
(require 'dragonruby-sprite-model)

(defun dragonruby--get-active-snippets ()
  "Collect all registered snippets from active modules."
  (let (all-snippets)
    ;; 1. Paths
    (when (and (boundp 'dragonruby-path-snippets)
               (or (not (boundp 'dragonruby-enable-paths)) dragonruby-enable-paths))
      (setq all-snippets (append all-snippets dragonruby-path-snippets)))
    
    ;; 2. Sprites
    (when (and (boundp 'dragonruby-sprite-snippets)
               (or (not (boundp 'dragonruby-enable-sprites)) dragonruby-enable-sprites))
      (setq all-snippets (append all-snippets dragonruby-sprite-snippets)))
    
    all-snippets))

(defun dragonruby--try-expand-snippet ()
  "Try to expand a snippet at point. Returns t if expanded."
  (let* ((line-start (line-beginning-position))
         (before (buffer-substring-no-properties line-start (point)))
         (active-snippets (dragonruby--get-active-snippets)))
    (cl-loop for (keyword . expansion) in active-snippets
             when (string-suffix-p keyword before)
             do (let ((start (- (point) (length keyword))))
                  (delete-region start (point))
                  (insert expansion)
                  (let ((pos (string-match-p "\"\"" expansion)))
                    (if pos
                        (backward-char (- (length expansion) (1+ pos)))
                      ;; If it's a path like "sprites/", put cursor before the last quote
                      (when (string-suffix-p "\"" expansion)
                        (backward-char 1)))))
             and return t
             finally return nil)))

(defun dragonruby--inside-string-p ()
  "Check if point is inside a string."
  (or (nth 3 (syntax-ppss))
      (save-excursion
        (let ((pos (point)))
          (and (search-backward "\"" (line-beginning-position) t)
               (progn
                 (goto-char pos)
                 (search-forward "\"" (line-end-position) t)))))))

(provide 'dragonruby-path-snippets)
;;; dragonruby-path-snippets.el ends here
