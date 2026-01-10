;;; dragonruby-font-completion.el --- CAPF for fonts -*- lexical-binding: t; -*-

(require 'dragonruby-font-fs)

(defun dragonruby-font-completion-at-point ()
  "Completion at point function for font paths.
Only activates inside strings that look like font paths."
  (when (nth 3 (syntax-ppss))  ; inside a string?
    (let* ((string-start (1+ (nth 8 (syntax-ppss))))
           (point-now (point))
           (content (buffer-substring-no-properties string-start point-now)))
      ;; Activate for font-like paths
      (when (and (> point-now string-start)
                 (or (string-prefix-p "fonts" content)
                     (string-prefix-p "fon" content)))
        (list string-start
              point-now
              (completion-table-dynamic
               (lambda (_) (dragonruby--get-all-fonts-in-project)))
              :exclusive 'no)))))

(defun dragonruby--setup-font-capf ()
  "Setup completion at point for fonts."
  (add-hook 'completion-at-point-functions #'dragonruby-font-completion-at-point 100 t))

(provide 'dragonruby-font-completion)
;;; dragonruby-font-completion.el ends here
