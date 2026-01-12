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
;; 🧠 FULL PURGE (No ghosts, no bytecode, no stale symbols)
;; ============================================================
(defun dragonruby--hard-reset ()
  "Completely wipe DragonRuby from Emacs memory."
  (interactive)

  ;; 1. Turn off mode everywhere
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (bound-and-true-p dragonruby-mode)
        (ignore-errors (dragonruby-mode -1)))))

  ;; 2. Remove all dragonruby-* features
  (setq features
        (cl-delete-if
         (lambda (f)
           (string-prefix-p "dragonruby-" (symbol-name f)))
         features))

  ;; 3. Kill all dragonruby-* symbols (except the root path)
  (mapatoms
   (lambda (sym)
     (let ((name (symbol-name sym)))
       (when (and (string-prefix-p "dragonruby-" name)
                  (not (string= name "dragonruby-project-root")))
         (ignore-errors (fmakunbound sym))
         (ignore-errors (makunbound sym))))))

  ;; 4. Delete all .elc
  (shell-command
   (format "find %s -name '*.elc' -delete"
           (shell-quote-argument dragonruby-project-root)))

  ;; 5. Clean load-history
  (setq load-history
        (cl-delete-if
         (lambda (x)
           (and (consp x)
                (string-match-p "dragonruby" (format "%s" x))))
         load-history))

  (garbage-collect)
  (message "🔥 DragonRuby memory purged"))

;; ============================================================
;; 🔥 TRUE HOT RELOAD
;; ============================================================
(defun dragonruby-hot-reload ()
  "Refresh the entire dragonruby-mode environment from source."
  (interactive)
  (let ((active-buffers (cl-loop for buf in (buffer-list)
                                 when (with-current-buffer buf (bound-and-true-p dragonruby-mode))
                                 collect buf)))
    ;; 0. Rebuild load-path (Emergency recovery)
    (let ((mod-dir (expand-file-name "modules" dragonruby-project-root)))
      (dolist (sub '("core" "sprites" "fonts" "audio" "colors" "paths" "concepts" "completion" "guide"))
        (add-to-list 'load-path (expand-file-name sub mod-dir))))

    ;; 1. Global Reset (Detach all active modules)
    (when (fboundp 'dragonruby-kernel-reset-live)
      (dragonruby-kernel-reset-live))

    ;; 2. Source Reload
    (let ((load-prefer-newer t)
          (byte-compile-warnings nil))
      (load-file (expand-file-name "dragonruby-mode.el" dragonruby-project-root)))

    ;; 3. Full Cycle Reboot
    (dolist (buf active-buffers)
      (with-current-buffer buf
        (dragonruby-mode -1)
        (dragonruby-mode 1)))

    (message "🐲 DragonRuby: Hot-Reload Cycle COMPLETE (Paths Restored)")))

;; ============================================================
;; 🧩 User Lego Profile
;; ============================================================
(setq dragonruby-enable-completion   t)   ; ✅ ON (Minimalist v0.7.2)
(setq dragonruby-enable-colors       nil) ; 🔴 OFF
(setq dragonruby-enable-sprites      nil) ; 🔴 OFF
(setq dragonruby-enable-sprite-tools nil) ; 🔴 OFF
(setq dragonruby-enable-fonts        nil) ; 🔴 OFF
(setq dragonruby-enable-font-tools   nil) ; 🔴 OFF
(setq dragonruby-enable-audio        nil) ; 🔴 OFF
(setq dragonruby-enable-paths        nil) ; 🔴 OFF
(setq dragonruby-enable-concepts     nil) ; 🔴 OFF
(setq dragonruby-enable-guide        nil) ; 🔴 OFF

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
