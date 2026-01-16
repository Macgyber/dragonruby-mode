;;; dragonruby-mode.el --- Semantic tooling for DragonRuby (Kernel Architecture) -*- lexical-binding: t; -*-

;; Author: Macgyber <esteban3261g@gmail.com>
;; Version: 0.7.4
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

(defun dragonruby--setup-load-path ()
  "Setup the load path for DragonRuby mode modules."
  (let* ((load-path-source (or load-file-name (buffer-file-name)))
         (plugin-root (and load-path-source (file-name-directory load-path-source))))
    (when plugin-root
      (let ((mod-dir (expand-file-name "modules" plugin-root)))
        ;; Core
        (add-to-list 'load-path (expand-file-name "core" mod-dir))
        
        ;; Features
        (dolist (dir '("sprites" "sprites/tools" "fonts" "fonts/tools" 
                       "audio" "colors" "paths" "concepts" "completion" "guide"))
          (add-to-list 'load-path (expand-file-name dir mod-dir)))
        
        ;; Stargate
        (dolist (dir '("stargate/emacs" "stargate/protocol" "stargate/sessions"))
          (add-to-list 'load-path (expand-file-name dir mod-dir)))))))

;; Initialize Load Path
(eval-and-compile
  (dragonruby--setup-load-path))

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
(require 'dragonruby-stargate-core)
(require 'dragonruby-guide)

;; -----------------------------------------------------------------------------
;; ⚙️ Configuration (Feature Flags)
;; -----------------------------------------------------------------------------

(defcustom dragonruby-enable-sprites t "Enable sprite system." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-sprite-tools t "Enable sprite tools." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-fonts t "Enable font system." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-font-tools t "Enable font tools." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-audio t "Enable audio system." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-colors t "Enable color system." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-picker t 
  "Enable color picker. ⚠️ EXPERIMENTAL: Interaction model may change." 
  :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-paths t "Enable path system." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-concepts t "Enable concept system." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-completion t "Enable completion system." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-stargate t "Enable Stargate time-traveling system." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-guide t "Enable knowledge guidance system." :type 'boolean :group 'dragonruby)

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
        ;; 1. Core & Scheduler (The Foundation)
        (dragonruby-enable 'core)
        (dragonruby-enable 'scheduler)
        
        ;; 2. Kernel Activation (Global Services)
        (let (enabled-msgs disabled-msgs)
          (cl-labels ((activate-mod (mod var &optional req-mod)
                        (let ((is-enabled (symbol-value var)))
                          (if is-enabled
                              (if (or (not req-mod) (eq (dragonruby-module-status req-mod) :active))
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
            ;; 11. stargate (The Mind Layer - Last to Boot)
            (activate-mod 'stargate 'dragonruby-enable-stargate)

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
  (dragonruby-kernel-buffer-cleanup)
  (message "【 🐉 】 DragonRuby Mode Shutdown Complete."))

(defcustom dragonruby-auto-activation-modes '(ruby-mode image-mode conf-mode)
  "List of major modes that trigger auto-detection of DragonRuby project."
  :type '(repeat symbol)
  :group 'dragonruby)

;;;###autoload
(defun dragonruby-maybe-enable ()
  "Enable `dragonruby-mode' only if in a DragonRuby project."
  (when (and (not dragonruby-mode)
             (or (member major-mode dragonruby-auto-activation-modes)
                 (let ((ext (file-name-extension (or (buffer-file-name) ""))))
                   (member ext '("rb" "png" "jpg" "jpeg" "gif" "bmp" "webp" "ttf" "otf" "wav" "ogg")))))
    (let ((root (dragonruby--find-project-root t))) ;; Always quiet in hooks
      (when root
        (dragonruby-mode 1)))))

;; Register hooks
(add-hook 'find-file-hook #'dragonruby-maybe-enable)
(add-hook 'ruby-mode-hook #'dragonruby-maybe-enable)
(add-hook 'image-mode-hook #'dragonruby-maybe-enable)

(defun dragonruby-reset-and-reload ()
  "Force a full system reset and reload from current configuration.
Use this to apply changes made to `.emacs` without restarting Emacs."
  (interactive)
  (message "🌀 DragonRuby: Syncing Reality...")
  ;; 0. Evaluation of user config (Look for .emacs in plugin root or use current buffer)
  (let* ((load-path-source (or load-file-name (buffer-file-name)))
         (plugin-root (and load-path-source (file-name-directory load-path-source)))
         (config-file (and plugin-root (expand-file-name ".emacs" plugin-root))))
    (if (and config-file (file-exists-p config-file))
        (load config-file)
      (when (string-match-p "\\.emacs$" (or (buffer-file-name) ""))
        (eval-buffer))))
  
  ;; 1. Global Reset (Kills all active activity)
  (dragonruby-kernel-system-shutdown)
  ;; 2. Re-evaluate the local mode file and core libs (to refresh registry)
  (load "dragonruby-mode")
  (load "dragonruby-utils")
  ;; 3. Scan all buffers and re-enable if applicable
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (dragonruby--find-project-root)
        (dragonruby-mode 1))))
  (message "✨ DragonRuby: Reality Synced. Configuration Applied."))

(message "【 🐉 】 DragonRuby Mode (Lego) LOADED.")

(provide 'dragonruby-mode)
;;; dragonruby-mode.el ends here
