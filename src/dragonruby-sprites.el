;;; dragonruby-sprites.el --- Sprite previews and completion -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'image-file)
(require 'dragonruby-core)

;; Load submodules
(require 'dragonruby-sprite-model      "sprites/dragonruby-sprite-model")
(require 'dragonruby-sprite-fs         "sprites/dragonruby-sprite-fs")
(require 'dragonruby-sprite-actions    "sprites/dragonruby-sprite-actions")
(require 'dragonruby-sprite-overlay    "sprites/dragonruby-sprite-overlay")
(require 'dragonruby-sprite-completion "sprites/dragonruby-sprite-completion")

(define-minor-mode dragonruby-sprite-mode
  "Sprite previews."
  :lighter ""
  (if dragonruby-sprite-mode
      (progn
        (add-hook 'after-change-functions #'dragonruby--after-sprite-change nil t)
        (add-hook 'window-configuration-change-hook #'dragonruby--refresh-sprites nil t)
        ;; Refresh sprites when font size changes (accessibility zoom)
        (add-hook 'text-scale-mode-hook #'dragonruby--refresh-sprites nil t)
        (dragonruby--setup-capf)
        (dragonruby--scan-sprites))
    (remove-hook 'after-change-functions #'dragonruby--after-sprite-change t)
    (remove-hook 'window-configuration-change-hook #'dragonruby--refresh-sprites t)
    (remove-hook 'text-scale-mode-hook #'dragonruby--refresh-sprites t)
    (remove-hook 'completion-at-point-functions #'dragonruby-sprite-completion-at-point t)
    (dragonruby--clear-sprite-overlays)))

(provide 'dragonruby-sprites)
;;; dragonruby-sprites.el ends here
