;;; dragonruby-impl.el --- DragonRuby Minor Mode Implementation  -*- lexical-binding: t; -*-

(require 'dragonruby-eldoc)

;;;###autoload
(define-minor-mode dragonruby-mode
  "Minor mode for assisting DragonRuby development."
  :lighter " DR"
  :group 'dragonruby
  (if dragonruby-mode
      (dragonruby-enable-eldoc)
    (kill-local-variable 'eldoc-documentation-function)))

;; ---------------------------------------------------------------------------
;; Ruby integration
;; ---------------------------------------------------------------------------

;;;###autoload
(defun dragonruby-mode-maybe-enable ()
  "Enable `dragonruby-mode` in DragonRuby Ruby files."
  (when (and (eq major-mode 'ruby-mode)
             (save-excursion
               (goto-char (point-min))
               (re-search-forward "def\\s-+tick\\s-*(args" nil t)))
    (dragonruby-mode 1)))

(add-hook 'ruby-mode-hook #'dragonruby-mode-maybe-enable)

(provide 'dragonruby-impl)
