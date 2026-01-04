;;; dragonruby-fonts.el --- Font services and viewer for DragonRuby -*- lexical-binding: t; -*-

(require 'dragonruby-core)

;; Logic from src/font/
(require 'dragonruby-font-model)
(require 'dragonruby-font-fs)
(require 'dragonruby-font-overlay)
(require 'dragonruby-font-completion)

;; UI from src/font-tools/
(require 'dragonruby-font-ui)

;; Font Viewer Major Mode
(define-derived-mode dragonruby-font-viewer-mode special-mode "DR-Font"
  "Major mode for viewing font files in DragonRuby."
  (setq cursor-type nil)
  (dragonruby--setup-font-header-line)
  (dragonruby-font-viewer-refresh))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.\\(ttf\\|otf\\)\\'" . dragonruby-font-viewer-mode))

;; Inline Font Visuals Minor Mode
(define-minor-mode dragonruby-font-mode
  "Visual feedback for fonts in DragonRuby."
  :lighter ""
  (if dragonruby-font-mode
      (progn
        (add-hook 'after-change-functions #'dragonruby--after-font-overlay-change nil t)
        (add-hook 'window-configuration-change-hook #'dragonruby--refresh-font-overlays nil t)
        (dragonruby--setup-font-capf)
        (dragonruby--scan-font-overlays))
    (remove-hook 'after-change-functions #'dragonruby--after-font-overlay-change t)
    (remove-hook 'window-configuration-change-hook #'dragonruby--refresh-font-overlays t)
    (dragonruby--clear-font-overlays)))

(provide 'dragonruby-fonts)
;;; dragonruby-fonts.el ends here
