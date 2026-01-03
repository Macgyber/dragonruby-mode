;;; dragonruby-dev.el --- Dev tools for hacking on dragonruby-mode

(defun dragonruby-hot-reload ()
  "Reloads the entire dragonruby-mode codebase from source.
Useful when developing the plugin itself."
  (interactive)
  (let* ((dev-dir (file-name-directory (or load-file-name buffer-file-name)))
         (root-dir (file-name-parent-directory dev-dir)) ;; ../
         (src-dir (expand-file-name "src" root-dir))
         (main-file (expand-file-name "dragonruby-mode.el" root-dir)))
    
    (message "🔥 Hot Reloading DragonRuby Mode from: %s" root-dir)
    
    ;; 1. Load all .el files in src/ recursively
    ;; We use directory-files-recursively
    (dolist (file (directory-files-recursively src-dir "\\.el$"))
      (unless (string-match-p "flycheck" file) ;; Skip flycheck files if any
        (load-file file)))

    ;; 2. Load the main entry point
    (load-file main-file)

    ;; 3. Restart mode in current buffer if active
    (when (bound-and-true-p dragonruby-mode)
      (dragonruby-mode -1)
      (dragonruby-mode 1))

    (message "✅ DragonRuby Mode Source Reloaded!")))

;; Bind this to a convenient key for development
(global-set-key (kbd "C-c C-r") 'dragonruby-hot-reload)
(global-set-key (kbd "<f5>") 'dragonruby-hot-reload)

;; Ensure F5 works in ruby-mode specifically (sometimes global keys are overshadowed)
(add-hook 'ruby-mode-hook 
          (lambda () 
            (local-set-key (kbd "<f5>") 'dragonruby-hot-reload)))

(message "DragonRuby Dev Tools Loaded. Press F5 or C-c C-r to Hot Reload.")
