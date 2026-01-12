;;; .emacs --- DragonRuby Local Dev Configuration (TRUE HOT RELOAD)

(require 'cl-lib)

;; ============================================================
;; 🐉 DragonRuby Project Root
;; ============================================================
;; Define la raíz del proyecto dinámicamente
(defvar dragonruby-project-root 
  (let ((path (or load-file-name buffer-file-name)))
    (if path (file-name-directory path) default-directory))
  "Root of the dragonruby-mode project.")

(when dragonruby-project-root
  (add-to-list 'load-path dragonruby-project-root))

;; ============================================================
;; 🧠 SURGICAL RESET (The Sovereign Kernel Way)
;; ============================================================
(defun dragonruby-hard-reset ()
  "OS-Level system purge. Halts everything and clears features."
  (interactive)
  (when (fboundp 'dragonruby-kernel-system-shutdown)
    (dragonruby-kernel-system-shutdown))
  
  ;; Clear Emacs features list to allow fresh 'require'
  (setq features
        (cl-delete-if (lambda (f) (string-prefix-p "dragonruby-" (symbol-name f))) features))
  
  (setq load-history
        (cl-delete-if (lambda (x) (and (consp x) (string-match-p "dragonruby" (format "%s" x)))) load-history))
  
  (garbage-collect)
  (message "🐲 DragonRuby: Clean slate. Ready for boot."))

;; ============================================================
;; 🔄 TRUE HOT RELOAD (Shutdown -> Unload -> Load -> Reboot)
;; ============================================================

(defun dragonruby-hot-reload ()
  "The Surgical Loop: Shutdown -> Unload -> Load -> Reboot."
  (interactive)
  (let ((active-buffers (cl-remove-if-not 
                         (lambda (b) (buffer-local-value 'dragonruby-mode b)) 
                         (buffer-list))))
    
    (message "🐲 DragonRuby: SURGICAL HOT RELOAD STARTING...")

    ;; 1. SHUTDOWN: Full system halt in ALL buffers
    (when (fboundp 'dragonruby-kernel-system-shutdown)
      (dragonruby-kernel-system-shutdown))

    ;; 2. CLEAR CACHES & UNLOAD
    (when (boundp 'dragonruby--audio-duration-cache)
      (clrhash dragonruby--audio-duration-cache))
    (setq features (cl-delete-if (lambda (f) (string-prefix-p "dragonruby-" (symbol-name f))) features))
    
    ;; 3. LOAD: Fresh code from disk (Cold Load)
    (let ((load-prefer-newer t)
          (byte-compile-warnings nil))
      ;; Force reload the main entry point
      (load (expand-file-name "dragonruby-mode.el" dragonruby-project-root) nil t)
      
      ;; Force reload critical modules to ensure logic update
      (let ((mod-dir (expand-file-name "modules" dragonruby-project-root)))
        (dolist (mod '("core/dragonruby-kernel.el"
                       "core/dragonruby-scheduler.el"
                       "core/dragonruby-utils.el"
                       "audio/dragonruby-audio-fs.el"
                       "audio/dragonruby-audio-overlay.el"))
          (let ((f (expand-file-name mod mod-dir)))
            (when (file-exists-p f) (load f nil t))))))

    ;; 4. REBOOT: re-enable in previously active buffers
    (dolist (buf active-buffers)
      (with-current-buffer buf
        (dragonruby-mode 1)))

    (message "🐲 DragonRuby: HOT RELOAD COMPLETE (True Surgical Cycle)")))

;; ============================================================
;; 🧩 User Lego Profile
;; ============================================================
(setq dragonruby-enable-completion   t)   ; ✅ ON
(setq dragonruby-enable-colors       t)   ; ✅ ON
(setq dragonruby-enable-sprites      t)   ; ✅ ON
(setq dragonruby-enable-sprite-tools t)   ; ✅ ON
(setq dragonruby-enable-fonts        t)   ; ✅ ON
(setq dragonruby-enable-font-tools   t)   ; ✅ ON
(setq dragonruby-enable-audio        t)   ; ✅ ON
(setq dragonruby-enable-paths        t)   ; ✅ ON
(setq dragonruby-enable-concepts     t)   ; ✅ ON
(setq dragonruby-enable-guide        t)   ; ✅ ON

;; ============================================================
;; 🧲 Activate on open Ruby buffers
;; ============================================================
(defun dragonruby--activate-in-all-ruby-buffers ()
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (or (derived-mode-p 'ruby-mode)
                     (derived-mode-p 'ruby-ts-mode)
                     (string-match-p "\\.rb$" (or (buffer-file-name) "")))
                 (not (bound-and-true-p dragonruby-mode)))
        (ignore-errors (dragonruby-mode 1))))))

;; ============================================================
;; 🧠 Initial boot
;; ============================================================
(condition-case err
    (require 'dragonruby-mode)
  (error (message "❌ DragonRuby boot failed: %s" err)))

(dragonruby--activate-in-all-ruby-buffers)

;; ============================================================
;; 🔧 Dev keys
;; ============================================================
(defun reload-local-emacs ()
  (interactive)
  (load-file (expand-file-name ".emacs" dragonruby-project-root))
  (message "♻️ Local profile reloaded and synchronized"))

(global-set-key (kbd "<f5>") #'reload-local-emacs)
(global-set-key (kbd "<f6>") #'dragonruby-hot-reload)
(global-set-key (kbd "<f12>") #'find-file)

;; ============================================================
;; 🧲 Hooks
;; ============================================================
(add-hook 'ruby-mode-hook #'dragonruby-mode)
(add-hook 'ruby-ts-mode-hook #'dragonruby-mode)

;; ============================================================
;; 📚 Alexandria Library (Org-mode Aesthetics)
;; ============================================================
(setq org-hide-emphasis-markers t)
(setq org-startup-indented t)
(setq org-ellipsis " ▾")

;; Títulos con jerarquía visual (Libro Sagrado)
(custom-set-faces
 '(org-level-1 ((t (:height 1.4 :weight bold :foreground "#61AFEF"))))
 '(org-level-2 ((t (:height 1.2 :weight bold :foreground "#ABB2BF"))))
 '(org-level-3 ((t (:height 1.1 :weight semi-bold :foreground "#DCDFE4")))))

(message "🚀 DragonRuby kernel online")

;;; .emacs ends here
