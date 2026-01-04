;;; dragonruby-image-tools.el --- Image viewing and editing tools Facade -*- lexical-binding: t; -*-

;; Facade that exposes the public interface for image tools.

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
    
    ;; Legacy/Alt zoom (Standard + and - are native to image-mode)
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
    (dragonruby--setup-image-header-line)))

(define-minor-mode dragonruby-image-tools-mode
  "DragonRuby Image Tools."
  :global t
  :group 'dragonruby
  (if dragonruby-image-tools-mode
      (add-hook 'image-mode-hook #'dragonruby--image-mode-hook)
    (remove-hook 'image-mode-hook #'dragonruby--image-mode-hook)))

(provide 'dragonruby-image-tools)
