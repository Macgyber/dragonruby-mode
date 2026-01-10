;;; dragonruby-sprite-completion.el --- CAPF for sprites -*- lexical-binding: t; -*-

;; RULE: This completion MUST NOT interfere with LSP, Corfu, Company, or native Emacs.
;; We achieve this by:
;; 1. Using :exclusive 'no (allows other CAPFs to contribute)
;; 2. Adding at depth 100 (runs LAST, after LSP and others)
;; 3. Only activating for obvious sprite paths (strings containing "sprites/")

(require 'dragonruby-sprite-fs)

(defun dragonruby-sprite-completion-at-point ()
  "Completion at point function for sprite paths.
Only activates inside strings that look like sprite paths.
Returns nil otherwise to let other CAPFs handle completion."
  (when (nth 3 (syntax-ppss))  ; inside a string?
    (let* ((string-start (1+ (nth 8 (syntax-ppss))))
           (point-now (point))
           (content (buffer-substring-no-properties string-start point-now)))
      ;; STRICT: Only activate for sprite-like paths
      (when (and (> point-now string-start)
                 (or (string-prefix-p "sprites" content)
                     (string-prefix-p "spr" content)))
        (list string-start
              point-now
              (completion-table-dynamic
               (lambda (_) (dragonruby--get-all-sprites-in-project)))
              :exclusive 'no)))))  ;; <-- Non-exclusive: allows LSP/Corfu/Company to also contribute

(defun dragonruby--setup-capf ()
  "Setup completion at point for sprites.
Added at depth 100 to run AFTER LSP, Corfu, Company, etc."
  ;; Depth 100 = run last, don't interfere with other completion systems
  (add-hook 'completion-at-point-functions #'dragonruby-sprite-completion-at-point 100 t))

(provide 'dragonruby-sprite-completion)
;;; dragonruby-sprite-completion.el ends here
