;;; dragonruby.el --- DragonRuby Cognitive Mode  -*- lexical-binding: t; -*-

;; Author: Macgyber <esteban3261g@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: games, dragonruby, tools, help
;; URL: https://github.com/Macgyber/dragonruby-mode

;;; Commentary:
;; This package integrates DragonRuby concepts directly into the editor.
;; It follows the 'Cognitive Amplifier' philosophy: helping you understand
;; the engine while you type, rather than just automating tasks.

;;; Code:

;; Add subdirectories to load-path
(eval-and-compile
  (let ((current-dir (file-name-directory (or load-file-name buffer-file-name))))
    (add-to-list 'load-path (expand-file-name "core" current-dir))
    (add-to-list 'load-path (expand-file-name "modules" current-dir))
    (add-to-list 'load-path (expand-file-name "ui" current-dir))
    (add-to-list 'load-path (expand-file-name "mode" current-dir))
    (add-to-list 'load-path (expand-file-name "concepts" current-dir))))

(require 'dragonruby-core-concepts)
(require 'dragonruby-registry)
(require 'dragonruby-eldoc)
(require 'dragonruby-args)
(require 'dragonruby-args-sub)
(require 'dragonruby-impl)

(provide 'dragonruby)

;;; dragonruby.el ends here
