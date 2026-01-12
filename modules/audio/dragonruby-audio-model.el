;;; dragonruby-audio-model.el --- Domain logic for audio -*- lexical-binding: t; -*-

(require 'dragonruby-core)


;; Register Audio-specific snippets
;; sou -> Universal filter for audio
(defconst dragonruby-audio-snippets
  '(("sou" . "\"sounds/\"")))

(provide 'dragonruby-audio-model)
;;; dragonruby-audio-model.el ends here
