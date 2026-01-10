;;; dragonruby-audio-model.el --- Domain logic for audio -*- lexical-binding: t; -*-

(require 'dragonruby-core)


;; Register Audio-specific snippets
(dragonruby-registry-register 'audio
  '(:snippets (("sou" . "\"sounds/.wav\""))))

(provide 'dragonruby-audio-model)
;;; dragonruby-audio-model.el ends here
