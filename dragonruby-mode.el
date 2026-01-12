;;; dragonruby-mode.el --- Semantic tooling for DragonRuby (Kernel Architecture) -*- lexical-binding: t; -*-

;; Author: Macgyber <esteban3261g@gmail.com>
;; Version: 0.7.2
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
    (add-to-list 'load-path (expand-file-name "guide" mod-dir))))

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
(require 'dragonruby-guide)

;; -----------------------------------------------------------------------------
;; ⚙️ Configuration (Feature Flags)
;; -----------------------------------------------------------------------------

(defcustom dragonruby-enable-sprites nil "Enable sprite system." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-sprite-tools nil "Enable sprite tools." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-fonts nil "Enable font system." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-font-tools nil "Enable font tools." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-audio nil "Enable audio system." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-colors nil "Enable color system." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-picker nil 
  "Enable color picker. ⚠️ EXPERIMENTAL: Interaction model may change." 
  :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-paths nil "Enable path system." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-concepts nil "Enable concept system." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-completion t "Enable completion system." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-guide nil "Enable knowledge guidance system." :type 'boolean :group 'dragonruby)

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
            (define-key map (kbd ".") #'dragonruby-completion-self-insert-dot)
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
        (let (enabled-msgs disabled-msgs)
          (cl-labels ((activate-mod (mod var &optional req-mod)
                        (let ((is-enabled (symbol-value var)))
                          (if is-enabled
                              (if (or (not req-mod) (dragonruby-module-status req-mod) (eq (dragonruby-module-status req-mod) :active))
                                  (progn
                                    (dragonruby-enable mod)
                                    (push (format "🧠 Kernel: Module [%s] ENABLED" mod) enabled-msgs))
                                (push (format "🧠 Kernel: Module [%s] DISABLED (requires [%s])" mod req-mod) disabled-msgs))
                            (push (format "🧠 Kernel: Module [%s] DISABLED" mod) disabled-msgs)))))

            ;; The User-Defined "Visual to Non-Visual" Order:
            ;; 1. colors
            (activate-mod 'colors 'dragonruby-enable-colors)
            ;; 2. audio
            (activate-mod 'audio 'dragonruby-enable-audio)
            ;; 3. sprites
            (activate-mod 'sprites 'dragonruby-enable-sprites)
            ;; 4. fonts
            (activate-mod 'fonts 'dragonruby-enable-fonts)
            ;; 5. paths
            (activate-mod 'paths 'dragonruby-enable-paths)
            ;; 6. completion
            (activate-mod 'completion 'dragonruby-enable-completion)
            ;; 7. concepts
            (activate-mod 'concepts 'dragonruby-enable-concepts)
            ;; 8. guide
            (activate-mod 'guide 'dragonruby-enable-guide)
            ;; 9. tools-font (font-tools)
            (activate-mod 'font-tools 'dragonruby-enable-font-tools 'fonts)
            ;; 10. tools-sprites (sprite-tools)
            (activate-mod 'sprite-tools 'dragonruby-enable-sprite-tools 'sprites)

            ;; --- Final Reporting (ENABLED first, then DISABLED) ---
            (dolist (msg (reverse enabled-msgs)) (message msg))
            (dolist (msg (reverse disabled-msgs)) (message msg))))
        ;; 3. Local Safety
        (setq-local enable-recursive-minibuffers t))
    (error
     (message "❌ BOOT FAILED: %s" err)
     (dragonruby-mode -1))))

(defun dragonruby--shutdown-session ()
  "Shutdown the local session."
  (dragonruby-scheduler-disable)
  (dragonruby-kernel-reset-live)
  (message "【 🐉 】 DragonRuby Mode Shutdown Complete."))

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
