;;; dragonruby-mode.el --- Semantic tooling for DragonRuby -*- lexical-binding: t; -*-

;; Author: Macgyber <esteban3261g@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: games, dragonruby, tools
;; URL: https://github.com/Macgyber/dragonruby-mode

;;; Commentary:
;; Semantic assistance for DragonRuby Game Toolkit projects.
;; Provides: color visualization, sprite previews, and path navigation.
;;
;; This is NOT an LSP. It does NOT replace Solargraph.
;; It adds visual semantics, not language intelligence.

;;; Code:

;; Setup load-path for src/ modules
(eval-and-compile
  (let ((src-dir (expand-file-name "src" (file-name-directory
                   (cond
                    (load-in-progress load-file-name)
                    ((and (boundp 'byte-compile-current-file)
                          byte-compile-current-file)
                     byte-compile-current-file)
                    (t (buffer-file-name)))))))
    (add-to-list 'load-path src-dir)))

;; Require modules from src/
(require 'dragonruby-core)
(require 'dragonruby-colors)
(require 'dragonruby-sprites)
(require 'dragonruby-paths)
(require 'dragonruby-image-tools)

;; Minor mode definition
(define-minor-mode dragonruby-mode
  "Semantic assistance for DragonRuby projects."
  :lighter " DR"
  :group 'dragonruby
  (if dragonruby-mode
      (progn
        (dragonruby-color-blocks-mode 1)
        (dragonruby-sprite-mode 1)
        (dragonruby-paths-mode 1)
        (dragonruby-image-tools-mode 1))
    (dragonruby-color-blocks-mode -1)
    (dragonruby-sprite-mode -1)
    (dragonruby-paths-mode -1)
    (dragonruby-image-tools-mode -1)))

;;;###autoload
(defun dragonruby-maybe-enable ()
  "Enable `dragonruby-mode' only if in a DragonRuby project.
A DragonRuby project is detected by the presence of `app/main.rb'
or a `.dragonruby' marker directory.

Usage in init.el:
  (add-hook \\='ruby-mode-hook #\\='dragonruby-maybe-enable)
  (add-hook \\='ruby-ts-mode-hook #\\='dragonruby-maybe-enable)"
  (when (dragonruby--find-project-root)
    (dragonruby-mode 1)))

(provide 'dragonruby-mode)
;;; dragonruby-mode.el ends here
