;;; dragonruby-mode.el --- Semantic tooling for DragonRuby (Kernel Architecture) -*- lexical-binding: t; -*-

;; Author: Macgyber <esteban3261g@gmail.com>
;; Version: 0.8.0
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

(defvar dragonruby-mode-root nil
  "The root directory of the dragonruby-mode plugin.")

(defun dragonruby--setup-load-path ()
  "Setup the load path for DragonRuby mode modules."
  (let* ((load-path-source (or load-file-name (buffer-file-name)))
         (plugin-root (and load-path-source (file-name-directory load-path-source))))
    (when plugin-root
      (setq dragonruby-mode-root plugin-root)
      (let ((mod-dir (expand-file-name "modules" plugin-root)))
        ;; Core
        (add-to-list 'load-path (expand-file-name "core" mod-dir))
        
        ;; Features
        (dolist (dir '("sprites" "sprites/tools" "fonts" "fonts/tools" 
                       "audio" "colors" "paths" "concepts" "completion" "guide"))
          (add-to-list 'load-path (expand-file-name dir mod-dir)))
        
        ;; Stargate
        (dolist (dir '("stargate/emacs" "stargate/protocol" "stargate/sessions" "stargate/ui"))
          (add-to-list 'load-path (expand-file-name dir mod-dir)))
        
        ;; DRDS (Dev Support) - DISABLED FOR NOW
        ;; (add-to-list 'load-path (expand-file-name "drds" mod-dir))
        ))))

;; Initialize Load Path
(eval-and-compile
  (dragonruby--setup-load-path))

;; 1. Initialize Kernel
(require 'dragonruby-kernel)

;; 2. Load Core Libraries
(require 'dragonruby-core)
(require 'dragonruby-utils)
;; (require 'dragonruby-symbols)
;; (require 'dragonruby-navigation)
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

(defcustom dragonruby-enable-sprites nil "Enable sprite system." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-sprite-tools nil "Enable sprite tools." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-sprites-lazy-visuals t
  "When non-nil, only render sprite previews when explicitly requested.
If nil, renders previews automatically for all paths."
  :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-fonts nil "Enable font system." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-font-tools nil "Enable font tools." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-audio nil "Enable audio system." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-colors nil "Enable color system." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-picker t 
  "Enable color picker. ⚠️ EXPERIMENTAL: Interaction model may change." 
  :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-paths nil "Enable path system." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-concepts nil "Enable concept system." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-completion t "Enable completion system." :type 'boolean :group 'dragonruby)
;; (defcustom dragonruby-completion-enable-project-symbols t
;;   "When non-nil, collect symbols from the project for enhanced autocompletion."
;;   :type 'boolean :group 'dragonruby)
;; (defcustom dragonruby-enable-navigation t "Enable DRDS navigation (xref)." :type 'boolean :group 'dragonruby)
(defcustom dragonruby-enable-stargate nil "Enable Stargate time-traveling system." :type 'boolean :group 'dragonruby)

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
            (define-key map (kbd "C-c C-s") dragonruby-stargate-mode-map)
            map)
  (if dragonruby-mode
      (dragonruby--boot-session)
    (dragonruby--shutdown-session)))

(defun dragonruby--boot-session ()
  "Boot the DragonRuby session for this buffer."
  (let ((reporter (make-progress-reporter "🐉 DragonRuby: Booting..." 0 13))
        (step 0))
    (condition-case err
        (progn
          ;; 1. Core (Step 1)
          (dragonruby-enable 'core)
          (setq step (1+ step))
          (progress-reporter-update reporter step "Core Foundation")

          ;; 2. Scheduler (Step 2)
          (dragonruby-enable 'scheduler)
          (setq step (1+ step))
          (progress-reporter-update reporter step "Scheduler Heartbeat")
          
          ;; 3. Activation & Categorization
          (let (active-foundations active-features active-tools disabled)
            (cl-labels ((check-mod (mod var type &optional req-mod)
                          (let ((is-enabled (symbol-value var)))
                            (setq step (1+ step))
                            (if is-enabled
                                (if (or (not req-mod) (eq (dragonruby-module-status req-mod) :active))
                                    (progn
                                      (dragonruby-enable mod)
                                      (let ((name (symbol-name mod)))
                                        (pcase type
                                          ('foundation (push name active-foundations))
                                          ('feature (push name active-features))
                                          ('tool (push name active-tools)))))
                                  (push (format "%s (missing %s)" mod req-mod) disabled))
                              (push (symbol-name mod) disabled))
                            (progress-reporter-update reporter step (symbol-name mod)))))

              ;; Define Categories
              (check-mod 'colors 'dragonruby-enable-colors 'tool)
              (check-mod 'audio 'dragonruby-enable-audio 'tool)
              (check-mod 'sprites 'dragonruby-enable-sprites 'feature)
              (check-mod 'fonts 'dragonruby-enable-fonts 'feature)
              (check-mod 'paths 'dragonruby-enable-paths 'feature)
              (check-mod 'completion 'dragonruby-enable-completion 'feature)
              (check-mod 'concepts 'dragonruby-enable-concepts 'tool)
              (check-mod 'guide 'dragonruby-enable-guide 'tool)
              (check-mod 'font-tools 'dragonruby-enable-font-tools 'tool 'fonts)
              (check-mod 'sprite-tools 'dragonruby-enable-sprite-tools 'tool 'sprites)
              (check-mod 'stargate 'dragonruby-enable-stargate 'tool)

              (progress-reporter-done reporter)

              ;; --- Categorized Reporting ---
              (message "--------------------------------------------------")
              (message "🧱 FOUNDATIONS  : core, scheduler")
              (when active-features
                (message "🚀 FEATURES ENABLED : %s" (mapconcat #'identity (reverse active-features) ", ")))
              (when active-tools
                (message "🛠️ TOOLS ENABLED    : %s" (mapconcat #'identity (reverse active-tools) ", ")))
              (when disabled
                (message "🌑 DISABLED         : %s" (mapconcat #'identity (reverse disabled) ", ")))
              (message "--------------------------------------------------")))

          ;; 4. Local Safety
          (setq-local enable-recursive-minibuffers t)
          
          ;; 5. Navigation (DRDS xref) - DISABLED
          ;; (when dragonruby-enable-navigation
          ;;   (add-hook 'xref-backend-functions #'dragonruby-navigation-xref-backend nil t))
          
          ;; 6. Symbol Maintenance - DISABLED
          ;; (add-hook 'after-save-hook #'dragonruby-symbols-on-save nil t)
          ;; Initial scan
          ;; (when (and dragonruby-completion-enable-project-symbols
          ;;            dragonruby-symbols-auto-scan)
          ;;   (dragonruby-symbols-scan-project))
          )

      (error
       (when reporter (progress-reporter-done reporter))
       (message "❌ BOOT FAILED at %d%%: %s" (floor (* (/ (float step) 13.0) 100)) err)
       (dragonruby-mode -1)))))

(defun dragonruby--shutdown-session ()
  "Shutdown the local session."
  (dragonruby-scheduler-disable)
  (dragonruby-kernel-buffer-cleanup)
  (message "【 🐉 】 DragonRuby Mode Shutdown Complete."))

(defcustom dragonruby-auto-activation-modes '(ruby-mode ruby-ts-mode image-mode conf-mode)
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
(add-hook 'ruby-ts-mode-hook #'dragonruby-maybe-enable)
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

(message "🐉 DragonRuby Mode (%d modules)" (hash-table-count dragonruby--modules))

(provide 'dragonruby-mode)
;;; dragonruby-mode.el ends here
