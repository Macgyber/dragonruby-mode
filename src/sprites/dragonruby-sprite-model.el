;;; dragonruby-sprite-model.el --- Domain logic for sprites -*- lexical-binding: t; -*-

(require 'dragonruby-assets)
(require 'dragonruby-registry)

;; Register Sprite-specific snippets for the "Interactive filtering" workflow
;; spr -> Universal filter
;; png/jpg/gif... -> Specific extension filters
(dragonruby-registry-register 'sprite
  '(:snippets (("spr" . "\"\""))))

(provide 'dragonruby-sprite-model)
;;; dragonruby-sprite-model.el ends here
