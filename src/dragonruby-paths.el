;;; dragonruby-paths.el --- Universal Code & Data Navigation (Facade) -*- lexical-binding: t; -*-

;; RULE: NO CAPF. Uses minibuffer to avoid conflicts with LSP/Corfu/Company.
;; Priority: User's installed plugins > DragonRuby > Native Emacs

(require 'dragonruby-core)
(require 'dragonruby-path-model "paths/dragonruby-path-model")
(require 'dragonruby-path-fs "paths/dragonruby-path-fs")
(require 'dragonruby-path-snippets "paths/dragonruby-path-snippets")
(require 'dragonruby-path-overlay "paths/dragonruby-path-overlay")
(require 'dragonruby-path-actions "paths/dragonruby-path-actions")
(require 'dragonruby-path-completion "paths/dragonruby-path-completion")

(defvar dragonruby-enable-path-completion)

;; --------------------------------------------------
;; MODE HOOKS
;; --------------------------------------------------

(defun dragonruby--after-path-change (beg end _len)
  "Fast path scanning after buffer change for immediate keyboard feedback."
  (save-match-data
    (let ((line-beg (save-excursion (goto-char beg) (line-beginning-position)))
          (line-end (save-excursion (goto-char end) (line-end-position))))
      (remove-overlays line-beg line-end 'dragonruby-path t)))
  (dragonruby--debounce 'paths #'dragonruby--scan-paths 0.05))

(defun dragonruby--refresh-paths ()
  "Refresh path overlays when buffer becomes visible."
  (when (and (boundp 'dragonruby-paths-mode) dragonruby-paths-mode
             (eq (current-buffer) (window-buffer)))
    (dragonruby--scan-paths)))

;; --------------------------------------------------
;; KEYMAP
;; --------------------------------------------------

(defvar dragonruby-paths-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c o") #'dragonruby-open-project-path)
    (define-key map (kbd "C-M-i") #'dragonruby-smart-complete)
    map)
  "Keymap for dragonruby-paths-mode.")

;; --------------------------------------------------
;; MODE DEFINITION
;; --------------------------------------------------

(define-minor-mode dragonruby-paths-mode
  "DragonRuby passive path navigation (minibuffer-based, LSP-safe).

Keybindings:
  C-M-i   Complete path at point (inside string)
  C-c o   Open any project file"
  :lighter ""
  :keymap dragonruby-paths-mode-map
  (if dragonruby-paths-mode
      (progn
        (add-hook 'after-change-functions #'dragonruby--after-path-change nil t)
        (add-hook 'window-configuration-change-hook #'dragonruby--refresh-paths nil t)
        (when dragonruby-enable-path-completion
          (add-hook 'completion-at-point-functions #'dragonruby-path-completion-at-point 90 t))
        (run-with-idle-timer 0.1 nil (lambda ()
                                       (when (bound-and-true-p dragonruby-paths-mode)
                                         (dragonruby--scan-paths)))))
    (remove-hook 'after-change-functions #'dragonruby--after-path-change t)
    (remove-hook 'window-configuration-change-hook #'dragonruby--refresh-paths t)
    (remove-hook 'completion-at-point-functions #'dragonruby-path-completion-at-point t)
    (dragonruby--clear-path-overlays)))

(provide 'dragonruby-paths)
;;; dragonruby-paths.el ends here
