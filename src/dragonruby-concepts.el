;;; dragonruby-concepts.el --- Concept Recognition Facade -*- lexical-binding: t; -*-

(require 'dragonruby-concept-visuals)

(defcustom dragonruby-enable-concepts t
  "Enable semantic concept highlighting."
  :type 'boolean
  :group 'dragonruby)

(define-minor-mode dragonruby-concepts-mode
  "Semantic highlighting for DragonRuby concepts."
  :lighter " 🧠"
  (if dragonruby-concepts-mode
      (progn
        (add-hook 'after-change-functions #'dragonruby--after-concept-change nil t)
        (add-hook 'window-configuration-change-hook #'dragonruby--refresh-concepts nil t)
        (dragonruby--scan-concepts))
    (remove-hook 'after-change-functions #'dragonruby--after-concept-change t)
    (remove-hook 'window-configuration-change-hook #'dragonruby--refresh-concepts t)
    (dragonruby--clear-concept-overlays)))

(provide 'dragonruby-concepts)
