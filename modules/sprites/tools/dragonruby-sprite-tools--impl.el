;;; dragonruby-sprite-tools--impl.el --- Image viewing and editing tools impl -*- lexical-binding: t; -*-

(require 'dragonruby-kernel)
(require 'dragonruby-image-modify)
(require 'dragonruby-image-view)
(require 'dragonruby-image-ui)

(defvar dragonruby-image-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Navigation (Timeline)
    (define-key map (kbd "<") #'dragonruby-image-undo)
    (define-key map (kbd ">") #'dragonruby-image-redo)
    (define-key map (kbd ",") #'dragonruby-image-undo)
    (define-key map (kbd ".") #'dragonruby-image-redo)
    
    ;; Transformations
    (define-key map (kbd "t") #'dragonruby-image-trim)
    (define-key map (kbd "z") #'dragonruby-image-compress)
    (define-key map (kbd "h") #'dragonruby-image-flip-h)
    (define-key map (kbd "v") #'dragonruby-image-flip-v)
    (define-key map (kbd "r") #'dragonruby-image-rotate)
    
    ;; Adjustments
    (define-key map (kbd "g") #'dragonruby-image-grayscale)
    (define-key map (kbd "n") #'dragonruby-image-invert)
    
    ;; Systems & Info
    (define-key map (kbd "i") #'dragonruby-image-info)
    (define-key map (kbd "e") #'dragonruby-image-open-external)
    (define-key map (kbd "p") #'dragonruby-image-to-png)
    (define-key map (kbd "c") #'dragonruby-image-crop)
    (define-key map (kbd "T") #'dragonruby-image-tint)
    (define-key map (kbd "R") #'dragonruby-image-hard-reset)
    
    map)
  "Keymap for DragonRuby Image Tools buffer.")

(define-minor-mode dragonruby-image-mode
  "Local minor mode for DragonRuby image buffers."
  :lighter " 🎨"
  :keymap dragonruby-image-mode-map)

(defun dragonruby--image-mode-hook ()
  "Activate toolbar and keybindings if inside a DragonRuby project."
  (when (and buffer-file-name (dragonruby--find-project-root))
    (dragonruby-image-mode 1)
    (dragonruby-image-init-history)
    ;; Always active by default
    (unless dragonruby--show-image-info (dragonruby-image-info))
    (dragonruby--setup-image-header-line)))

;;;###autoload
(defun dragonruby-sprite-tools-enable ()
  "Enable Sprite Tools."
  (dragonruby-kernel-register-hook 'image-mode-hook #'dragonruby--image-mode-hook)
  (message "🛠️ Sprite Tools Enabled"))

;;;###autoload
(defun dragonruby-sprite-tools-disable ()
  "Disable Sprite Tools."
  (remove-hook 'image-mode-hook #'dragonruby--image-mode-hook)
  (message "🛠️ Sprite Tools Disabled"))

(provide 'dragonruby-sprite-tools--impl)
;;; dragonruby-sprite-tools--impl.el ends here
