;;; dragonruby-mode.el --- Semantic tooling for DragonRuby (Kernel Architecture) -*- lexical-binding: t; -*-

;; Author: Macgyber <esteban3261g@gmail.com>
;; Version: 0.7.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: games, dragonruby, tools
;; URL: https://github.com/Macgyber/dragonruby-mode

;;; Commentary:
;; 🐲 DragonRuby Mode (Lego Architecture)
;;
;; This is the entry point. It initializes the Kernel and registers modules.
;; Actual feature activation is handled by the Kernel based on configuration.

;;; Code:

;; -----------------------------------------------------------------------------
;; 🧱 Bootloader (Load Path & Registry)
;; -----------------------------------------------------------------------------

(eval-and-compile
  (let* ((load-file-path (cond
                          (load-in-progress load-file-name)
                          ((and (boundp 'byte-compile-current-file)
                                byte-compile-current-file)
                           byte-compile-current-file)
                          (t (buffer-file-name))))
         (root-dir (file-name-directory load-file-path))
         (mod-dir (expand-file-name "modules" root-dir)))
    
    ;; Core
    (add-to-list 'load-path (expand-file-name "core" mod-dir))
    
    ;; Features
    (add-to-list 'load-path (expand-file-name "sprites" mod-dir))
    (add-to-list 'load-path (expand-file-name "sprites/tools" mod-dir))
    (add-to-list 'load-path (expand-file-name "fonts" mod-dir))
    (add-to-list 'load-path (expand-file-name "fonts/tools" mod-dir))
    (add-to-list 'load-path (expand-file-name "audio" mod-dir))
    (add-to-list 'load-path (expand-file-name "colors" mod-dir))
    (add-to-list 'load-path (expand-file-name "paths" mod-dir))
    (add-to-list 'load-path (expand-file-name "concepts" mod-dir))
    (add-to-list 'load-path (expand-file-name "completion" mod-dir))
    (add-to-list 'load-path (expand-file-name "docs" mod-dir))))

;; 1. Initialize Kernel
(require 'dragonruby-kernel)

;; 2. Load Core Libraries
(require 'dragonruby-core)
(require 'dragonruby-utils)
(require 'dragonruby-scheduler)

;; 3. Register Modules (Manifests)
(require 'dragonruby-sprites)
(require 'dragonruby-sprite-tools)
(require 'dragonruby-fonts)
(require 'dragonruby-font-tools)
(require 'dragonruby-audio)
(require 'dragonruby-colors)
(require 'dragonruby-paths)
(require 'dragonruby-concepts)
(require 'dragonruby-completion)
(require 'dragonruby-docs)

;; -----------------------------------------------------------------------------
;; ⚙️ Configuration (Feature Flags)
;; -----------------------------------------------------------------------------

(defcustom dragonruby-enable-sprites t "Enable sprite system." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-sprite-tools t "Enable sprite tools." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-fonts t "Enable font system." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-font-tools t "Enable font tools." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-audio t "Enable audio system." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-colors t "Enable color system." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-paths t "Enable path system." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-concepts t "Enable concept system." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-completion t "Enable completion system." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-docs t "Enable documentation system." :type 'boolean :group 'dragonruby)

;; -----------------------------------------------------------------------------
;; 🎮 Session Manager (The Mode)
;; -----------------------------------------------------------------------------

;;;###autoload
(define-minor-mode dragonruby-mode
  "Semantic assistance for DragonRuby projects (Lego Architecture)."
  :lighter " 🐲"
  :group 'dragonruby
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-.") #'completion-at-point)
            map)
  (if dragonruby-mode
      (dragonruby--boot-session)
    (dragonruby--shutdown-session)))

(defun dragonruby--boot-session ()
  "Boot the DragonRuby session for this buffer."
  (condition-case err
      (progn
        ;; 1. Scheduler (Per Buffer)
        (dragonruby-scheduler-enable)
        
        ;; 2. Kernel Activation (Global Services)
        (when dragonruby-enable-sprites (dragonruby-enable 'sprites))
        (when dragonruby-enable-sprite-tools (dragonruby-enable 'sprite-tools))
        (when dragonruby-enable-fonts (dragonruby-enable 'fonts))
        (when dragonruby-enable-font-tools (dragonruby-enable 'font-tools))
        (when dragonruby-enable-audio (dragonruby-enable 'audio))
        (when dragonruby-enable-colors (dragonruby-enable 'colors))
        (when dragonruby-enable-paths (dragonruby-enable 'paths))
        (when dragonruby-enable-concepts (dragonruby-enable 'concepts))
        (when dragonruby-enable-completion (dragonruby-enable 'completion))
        (when dragonruby-enable-docs (dragonruby-enable 'docs))

        ;; 3. Local Safety
        (setq-local enable-recursive-minibuffers t))
    (error
     (message "❌ BOOT FAILED: %s" err)
     (dragonruby-mode -1))))

(defun dragonruby--shutdown-session ()
  "Shutdown the local session."
  (dragonruby-scheduler-disable))

;;;###autoload
(defun dragonruby-maybe-enable ()
  "Enable `dragonruby-mode' only if in a DragonRuby project."
  (let ((root (dragonruby--find-project-root)))
    (if (and (not dragonruby-mode) root)
        (dragonruby-mode 1))))

;; Register hooks
(add-hook 'find-file-hook #'dragonruby-maybe-enable)
(add-hook 'ruby-mode-hook #'dragonruby-maybe-enable)

(message "【 🐉 】 DragonRuby Mode (Lego) LOADED.")

(provide 'dragonruby-mode)
;;; dragonruby-mode.el ends here
