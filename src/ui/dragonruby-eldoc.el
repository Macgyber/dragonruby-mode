;;; dragonruby-eldoc.el --- Eldoc support for DragonRuby concepts  -*- lexical-binding: t; -*-

(require 'eldoc)
(require 'dragonruby-registry)

(defun dragonruby--symbol-at-point ()
  "Return the symbol at point, handling Ruby method calls like 'args.state'.
Matches:
- word (e.g. 'args')
- object.method (e.g. 'args.state', 'args.outputs')
- object.method.submethod (e.g. 'args.inputs.keyboard')"
  (let ((symbol-at-point (thing-at-point 'symbol)))
    (when symbol-at-point
      (save-excursion
        ;; Try to look back for dotted parents
        (if (looking-back "\\(\\w+\\)\\." (line-beginning-position))
            (let ((parent (match-string 1)))
              (concat parent "." symbol-at-point))
          symbol-at-point)))))

(defun dragonruby-eldoc-function ()
  "Eldoc function for DragonRuby concepts."
  (let* ((symbol (dragonruby--symbol-at-point))
         (concept (and symbol
                       (dragonruby-get-concept symbol))))
    (when concept
      (dragonruby-concept-definition concept))))

(defun dragonruby-enable-eldoc ()
  "Enable DragonRuby Eldoc support in the current buffer."
  (setq-local eldoc-documentation-function
              #'dragonruby-eldoc-function)
  (eldoc-mode 1))

(provide 'dragonruby-eldoc)
