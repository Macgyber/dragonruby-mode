;;; dragonruby-path-snippets.el --- Snippet expansion -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'dragonruby-path-model)

(defun dragonruby--try-expand-snippet ()
  "Try to expand a snippet at point. Returns t if expanded."
  (let* ((line-start (line-beginning-position))
         (before (buffer-substring-no-properties line-start (point))))
    (cl-loop for (keyword . expansion) in dragonruby--require-snippets
             when (string-suffix-p keyword before)
             do (let ((start (- (point) (length keyword))))
                  (delete-region start (point))
                  (insert expansion)
                  (let ((pos (string-match-p "\"\"" expansion)))
                    (when pos
                      (backward-char (- (length expansion) (1+ pos))))))
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
