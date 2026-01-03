;;; dragonruby-mode.el --- Semantic tooling for DragonRuby -*- lexical-binding: t; -*-

;; Author: Macgyber <esteban3261g@gmail.com>
;; Version: 0.5.5
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
;; Setup load-path for src/ modules and submodules
(eval-and-compile
  (let* ((load-file-path (cond
                          (load-in-progress load-file-name)
                          ((and (boundp 'byte-compile-current-file)
                                byte-compile-current-file)
                           byte-compile-current-file)
                          (t (buffer-file-name))))
         (src-dir (expand-file-name "src" (file-name-directory load-file-path)))
         (sprites-dir (expand-file-name "sprites" src-dir)))
    
    (add-to-list 'load-path src-dir)
    (add-to-list 'load-path (expand-file-name "core" src-dir))
    (add-to-list 'load-path (expand-file-name "colors" src-dir))
    (add-to-list 'load-path sprites-dir)
    (add-to-list 'load-path (expand-file-name "paths" src-dir))
    (add-to-list 'load-path (expand-file-name "image-tools" src-dir))
    (add-to-list 'load-path (expand-file-name "concepts" src-dir))))

;; Require modules from src/
(require 'dragonruby-core)
(require 'dragonruby-colors)
(require 'dragonruby-sprites)
(require 'dragonruby-paths)
(require 'dragonruby-image-tools)
(require 'dragonruby-docs)
(require 'dragonruby-concepts)

;; --- FEATURE FLAGS ---

(defcustom dragonruby-enable-colors t
  "Enable semantic color highlighting."
  :type 'boolean
  :group 'dragonruby)

(defcustom dragonruby-enable-sprites t
  "Enable sprite previews and autocompletion."
  :type 'boolean
  :group 'dragonruby)

(defcustom dragonruby-enable-paths t
  "Enable path navigation."
  :type 'boolean
  :group 'dragonruby)

(defcustom dragonruby-enable-path-completion t
  "Enable contextual path autocompletion in requires and strings."
  :type 'boolean
  :group 'dragonruby)

(defcustom dragonruby-enable-image-tools t
  "Enable image editing tools (zoom, rotate, etc)."
  :type 'boolean
  :group 'dragonruby)

(defcustom dragonruby-enable-docs t
  "Enable documentation lookup system."
  :type 'boolean
  :group 'dragonruby)

(defcustom dragonruby-enable-picker nil
  "Enable interactive color picker (experimental).
When nil, color swatches are read-only."
  :type 'boolean
  :group 'dragonruby)

(defcustom dragonruby-enable-concepts t
  "Enable semantic concept highlighting."
  :type 'boolean
  :group 'dragonruby)

(defcustom dragonruby-experimental-smart-jump nil
  "Enable experimental Smart Source Jumping."
  :type 'boolean
  :group 'dragonruby)

;; Minor mode definition
(define-minor-mode dragonruby-mode
  "Semantic assistance for DragonRuby projects."
  :lighter " DragonRuby"
  :group 'dragonruby
  (if dragonruby-mode
      (condition-case err
          (progn
            (when dragonruby-enable-colors (dragonruby-color-blocks-mode 1))
            (when dragonruby-enable-sprites (dragonruby-sprite-mode 1))
            (when dragonruby-enable-paths (dragonruby-paths-mode 1))
            (when dragonruby-enable-image-tools (dragonruby-image-tools-mode 1))
            (when dragonruby-enable-docs (dragonruby-docs-mode 1))
            (when dragonruby-enable-concepts (dragonruby-concepts-mode 1)))
        (error
         (dragonruby--warn-in-development (format "Error en Activación: %s" err))
         (dragonruby-mode -1)))
    ;; Always disable everything when turning off the main mode, just to be safe/clean
    (dragonruby-color-blocks-mode -1)
    (dragonruby-sprite-mode -1)
    (dragonruby-paths-mode -1)
    (dragonruby-image-tools-mode -1)
    (dragonruby-docs-mode -1)
    (dragonruby-concepts-mode -1)
    (setq dragonruby--project-root-cache nil)))

;;;###autoload
(defun dragonruby-maybe-enable ()
  "Enable `dragonruby-mode' only if in a DragonRuby project.
A DragonRuby project is detected by the presence of `app/main.rb'
or a `.dragonruby' marker directory.

Usage in init.el:
  (add-hook \\='ruby-mode-hook #\\='dragonruby-maybe-enable)
  (add-hook \\='ruby-ts-mode-hook #\\='dragonruby-maybe-enable)"
  (when (and (not dragonruby-mode)
             (dragonruby--find-project-root))
    (dragonruby-mode 1)))

;; Auto-enable for already-open Ruby buffers when plugin loads
(defun dragonruby--enable-in-existing-buffers ()
  "Enable dragonruby-mode in existing Ruby buffers."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (derived-mode-p 'ruby-mode 'ruby-ts-mode)
                 (not dragonruby-mode))
        (dragonruby-maybe-enable)))))

;; Run once when plugin loads
(dragonruby--enable-in-existing-buffers)


(provide 'dragonruby-mode)
;;; dragonruby-mode.el ends here
