;;; dragonruby-path-snippets.el --- Snippet expansion -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'dragonruby-registry)
(require 'dragonruby-path-model)

(defun dragonruby--get-active-snippets ()
  "Collect all registered snippets from active modules."
  (let (all-snippets)
    (dolist (id (dragonruby-registry-all-ids))
      (let* ((feature-name (symbol-name id))
             ;; If it already ends in 's', don't add another 's'
             (normalized-id (if (string-suffix-p "s" feature-name)
                                feature-name
                              (concat feature-name "s")))
             (feature-flag (intern (format "dragonruby-enable-%s" normalized-id)))
             (is-enabled (if (boundp feature-flag) (symbol-value feature-flag) t))
             (snippets (dragonruby-registry-get id :snippets)))
        (when (and is-enabled snippets)
          (setq all-snippets (append all-snippets snippets)))))
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
