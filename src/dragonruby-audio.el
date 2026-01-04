;;; dragonruby-audio.el --- Audio services for DragonRuby -*- lexical-binding: t; -*-

(require 'dragonruby-core)
(require 'dragonruby-audio-model)
(require 'dragonruby-audio-fs)
(require 'dragonruby-audio-overlay)

(defvar dragonruby--audio-scan-timer nil
  "Timer for debounced audio scanning.")

(defun dragonruby--refresh-audio-overlays (&rest _)
  "Debounced refresh of audio overlays."
  (when dragonruby--audio-scan-timer
    (cancel-timer dragonruby--audio-scan-timer))
  (setq dragonruby--audio-scan-timer
        (run-with-idle-timer 0.8 nil
          (lambda (buf)
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (dragonruby--scan-audio-overlays))))
          (current-buffer))))

(define-minor-mode dragonruby-audio-mode
  "Audio tooling for DragonRuby."
  :lighter ""
  (if dragonruby-audio-mode
      (progn
        (add-hook 'after-change-functions #'dragonruby--refresh-audio-overlays nil t)
        (add-hook 'window-configuration-change-hook #'dragonruby--refresh-audio-overlays nil t)
        (dragonruby--scan-audio-overlays))
    (remove-hook 'after-change-functions #'dragonruby--refresh-audio-overlays t)
    (remove-hook 'window-configuration-change-hook #'dragonruby--refresh-audio-overlays t)
    (dragonruby--clear-audio-overlays)))

(provide 'dragonruby-audio)
;;; dragonruby-audio.el ends here
