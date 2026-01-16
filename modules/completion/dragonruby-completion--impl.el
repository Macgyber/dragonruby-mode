;;; dragonruby-completion--impl.el --- The Contract Enforcement implementation -*- lexical-binding: t; -*-

(require 'dragonruby-kernel)
(require 'dragonruby-core)
(require 'dragonruby-completion-yaml)
(require 'dragonruby-completion-backend)

(defcustom dragonruby-completion-file "dragonruby_api.yml"
  "Name of the contract file to look for."
  :type 'string
  :group 'dragonruby)

(defvar-local dragonruby-completion--active-mode nil)

;;;###autoload
(defun dragonruby-completion-enable ()
  "Enable API Contract completion."
  ;; 1. Find the contract file
  (let* ((root (dragonruby--find-project-root))
         (local-path (and root (expand-file-name dragonruby-completion-file root)))
         ;; Fallback: Look in the plugin's root directory (Reliable method)
         (plugin-dir (file-name-directory (or (locate-library "dragonruby-mode") 
                                              (expand-file-name "../.." (file-name-directory (or load-file-name buffer-file-name))))))
         (global-path (and plugin-dir (expand-file-name dragonruby-completion-file plugin-dir)))
         ;; Prioritize Local > Global
         (final-path (cond
                      ((and local-path (file-exists-p local-path)) local-path)
                      ((and global-path (file-exists-p global-path)) global-path)
                      (t nil))))
    
    (if final-path
        (progn
          ;; 2. Parse and Load the Contract
          (dragonruby-completion-backend-init final-path)
          
          ;; 3. Register CAPF
          (dragonruby-kernel-register-hook 'completion-at-point-functions #'dragonruby-completion-at-point t)
          
          (setq dragonruby-completion--active-mode t)
          (let ((type (if (equal final-path local-path) "Local" "Global")))
            (message "🐉 [Contract] Loaded (%s): %s" type (file-name-nondirectory final-path))))
      
      (message "ℹ️ [Contract] No '%s' found. API Autocomplete disabled." dragonruby-completion-file))))

;;;###autoload
(defun dragonruby-completion-disable ()
  "Disable API Contract completion."
  (remove-hook 'completion-at-point-functions #'dragonruby-completion-at-point t)
  (setq dragonruby-completion--active-mode nil)
  (setq dragonruby-completion--contract nil)
  (setq dragonruby-completion--roots nil)
  (message "🐉 [Contract] Completion Disabled"))

;;;###autoload
(defun dragonruby-completion-self-insert-dot (arg)
  "Insert a dot and trigger completion if it follows a valid contract chain."
  (interactive "p")
  (let* ((line-start (line-beginning-position))
         (prefix-before (buffer-substring-no-properties line-start (point))))
    (self-insert-command arg)
    (when (and (bound-and-true-p dragonruby-mode) 
               (bound-and-true-p dragonruby-enable-completion)
               (not (nth 8 (syntax-ppss)))) ;; Not in string or comment
      ;; Check for a valid chain match RIGHT BEFORE the dot we just inserted
      (when (string-match "\\(?:^\\|[^a-zA-Z0-9_$@]\\)\\([a-zA-Z0-9_$@]+\\(\\.[a-zA-Z0-9_$@]+\\)*\\)\\.$" 
                          (buffer-substring-no-properties line-start (point)))
        (let* ((full-match (match-string 1 (buffer-substring-no-properties line-start (point))))
               (clean-chain (substring full-match 0 -1))) ;; Remove the trailing dot
          (when (dragonruby-completion-backend-valid-chain-p clean-chain)
            (completion-at-point)))))))

(provide 'dragonruby-completion--impl)
;;; dragonruby-completion--impl.el ends here
