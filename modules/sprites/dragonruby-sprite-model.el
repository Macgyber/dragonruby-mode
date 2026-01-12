;;; dragonruby-sprite-model.el --- Domain logic for sprites -*- lexical-binding: t; -*-

(require 'dragonruby-core)


;; Register Sprite-specific snippets for the "Interactive filtering" workflow
;; spr -> Universal filter
;; png/jpg/gif... -> Specific extension filters
;; Sprite-specific snippets
(defconst dragonruby-sprite-snippets
  '(("spr" . "\"sprites/\"")))

;;; Configuration
(defvar dragonruby-supported-sprites dragonruby-image-exts
  "Alias for supported image extensions (Inherited from Core types).")

(defconst dragonruby-unsupported-sprites '("tiff" "ico" "psd" "ase" " aseprite")
  "Extensions recognized but not supported for preview.")

(provide 'dragonruby-sprite-model)
;;; dragonruby-sprite-model.el ends here
