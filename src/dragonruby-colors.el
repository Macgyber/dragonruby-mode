;;; dragonruby-colors.el --- Smart Color Scanning (RGBA-aware, Non-Intrusive) -*- lexical-binding: t; -*-

(require 'dragonruby-core)
(require 'dragonruby-color-utils)
(require 'dragonruby-color-visuals)
(require 'dragonruby-color-scanner)

(defgroup dragonruby-colors nil
  "Color visualization for DragonRuby."
  :group 'tools)

(defun dragonruby--after-color-change (beg end _len)
  "Trigger color scan after buffer change with debounce.
Cleans up overlays in the entire modified line factor to avoid 'visual ghosts' 
of orphaned overlays in multiline hashes or arrays."
  (save-match-data
    (let ((line-beg (save-excursion (goto-char beg) (line-beginning-position)))
          (line-end (save-excursion (goto-char end) (line-end-position))))
      (remove-overlays line-beg line-end 'dragonruby-color t)))
  (dragonruby--debounce 'colors #'dragonruby--scan-colors 0.25))

(defun dragonruby--refresh-colors ()
  "Refresh colors when window configuration changes."
  (when (and (bound-and-true-p dragonruby-color-blocks-mode)
             (eq (current-buffer) (window-buffer)))
    (dragonruby--scan-colors)))

;;;###autoload
(define-minor-mode dragonruby-color-blocks-mode
  "Passive color visualization for DragonRuby buffers."
  :lighter ""
  (if dragonruby-color-blocks-mode
      (progn
        (add-hook 'after-change-functions
                  #'dragonruby--after-color-change nil t)
        (add-hook 'window-configuration-change-hook
                  #'dragonruby--refresh-colors nil t)
        ;; The initial scan is triggered after a short delay to ensure buffer is ready
        (run-with-idle-timer 0.1 nil (lambda () 
                                       (when (bound-and-true-p dragonruby-color-blocks-mode)
                                         (dragonruby--scan-colors)))))
    (remove-hook 'after-change-functions
                 #'dragonruby--after-color-change t)
    (remove-hook 'window-configuration-change-hook
                 #'dragonruby--refresh-colors t)
    (dragonruby--clear-color-overlays)))

(provide 'dragonruby-colors)
