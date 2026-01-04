;;; dragonruby-font-model.el --- Domain logic for fonts -*- lexical-binding: t; -*-

(require 'dragonruby-assets)
(require 'dragonruby-registry)

;; Register Font-specific snippets
;; fon -> Universal filter for fonts
(dragonruby-registry-register 'font
  '(:snippets (("fon" . "\"fonts/.ttf\""))))

(provide 'dragonruby-font-model)
;;; dragonruby-font-model.el ends here
