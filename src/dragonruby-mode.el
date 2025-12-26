;;; dragonruby-mode.el --- Core minor mode for DragonRuby semantic tooling -*- lexical-binding: t; -*-

(require 'dragonruby-colors)
(require 'dragonruby-sprites)
(require 'dragonruby-paths)

(define-minor-mode dragonruby-mode
  "Core DragonRuby semantic mode."
  :lighter " DR"
  (if dragonruby-mode
      (progn
        (dragonruby-color-blocks-mode 1)
        (dragonruby-sprite-mode 1)
        (dragonruby-paths-mode 1))
    (progn
      (dragonruby-color-blocks-mode -1)
      (dragonruby-sprite-mode -1)
      (dragonruby-paths-mode -1))))

(provide 'dragonruby-mode)
