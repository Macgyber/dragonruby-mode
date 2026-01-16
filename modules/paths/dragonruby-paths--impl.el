;;; dragonruby-paths--impl.el --- Universal Code & Data Navigation implementation -*- lexical-binding: t; -*-

(require 'dragonruby-kernel)
(require 'dragonruby-core)
(require 'dragonruby-path-model)
(require 'dragonruby-path-fs)
(require 'dragonruby-path-snippets)
(require 'dragonruby-path-overlay)
(require 'dragonruby-path-actions)
(require 'dragonruby-path-completion)

(defvar dragonruby-enable-path-completion t)

(defvar dragonruby-paths-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c o") #'dragonruby-open-project-path)
    (define-key map (kbd "C-M-i") #'dragonruby-smart-complete)
    map)
  "Keymap for dragonruby-paths module.")

(define-minor-mode dragonruby-paths-mode
  "Local minor mode for DragonRuby paths."
  :lighter ""
  :keymap dragonruby-paths-mode-map)

(defun dragonruby--paths-hook ()
  (dragonruby-paths-mode 1))

;;;###autoload
(defun dragonruby-paths-enable ()
  "Enable path services."
  (dragonruby-kernel-register-hook 'dragonruby-scan-hook #'dragonruby--scan-paths t)
  ;; Hook keymap activation (Global)
  (dragonruby-kernel-register-hook 'ruby-mode-hook #'dragonruby--paths-hook)
  (when (eq major-mode 'ruby-mode) (dragonruby-paths-mode 1))
  
  (when dragonruby-enable-path-completion
    (dragonruby-kernel-register-hook 'completion-at-point-functions #'dragonruby-path-completion-at-point t))
  
  ;; Force an initial scan pulse
  (setq-local dragonruby--buffer-state 'dirty)
  
  (message "📂 Paths Module Enabled"))

;;;###autoload
(defun dragonruby-paths-disable ()
  "Disable path services."
  (remove-hook 'dragonruby-scan-hook #'dragonruby--scan-paths t)
  (remove-hook 'ruby-mode-hook #'dragonruby--paths-hook)
  (remove-hook 'completion-at-point-functions #'dragonruby-path-completion-at-point t)
  (dragonruby--clear-path-overlays)
  (dragonruby-paths-mode -1)
  (message "📂 Paths Module Disabled"))

(provide 'dragonruby-paths--impl)
;;; dragonruby-paths--impl.el ends here
