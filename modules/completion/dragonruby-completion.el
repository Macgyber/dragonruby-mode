;;; dragonruby-completion.el --- The Contract Enforcement Module -*- lexical-binding: t; -*-

(require 'dragonruby-kernel)
(require 'dragonruby-core)
(require 'dragonruby-completion-yaml)
(require 'dragonruby-completion-backend)

(defcustom dragonruby-completion-file "dragonruby_api.yml"
  "Name of the contract file to look for."
  :type 'string
  :group 'dragonruby)

(defvar-local dragonruby-completion--active-mode nil)

;; 🧱 LEGACY MINOR MODE REMOVED

;; -----------------------------------------------------------------------------
;; 🧬 Lifecycle
;; -----------------------------------------------------------------------------

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
          (add-hook 'completion-at-point-functions #'dragonruby-completion-at-point 50 t)
          
          (setq dragonruby-completion--active-mode t)
          (let ((type (if (equal final-path local-path) "Local" "Global")))
            (message "🐉 [Contract] Loaded (%s): %s" type (file-name-nondirectory final-path))))
      
      (message "ℹ️ [Contract] No '%s' found. API Autocomplete disabled." dragonruby-completion-file))))

(defun dragonruby-completion-disable ()
  "Disable API Contract completion."
  (remove-hook 'completion-at-point-functions #'dragonruby-completion-at-point t)
  (setq dragonruby-completion--active-mode nil)
  (setq dragonruby-completion--contract nil)
  (setq dragonruby-completion--roots nil)
  (message "🐉 [Contract] Completion Disabled"))

;; -----------------------------------------------------------------------------
;; 📜 Manifest
;; -----------------------------------------------------------------------------

(dragonruby-register-module
 :name 'completion
 :type :main
 :namespace "dragonruby-completion-"
 :provides '(:completion)
 :requires nil
 :entry-point 'dragonruby-completion
 :enable-fn #'dragonruby-completion-enable
 :disable-fn #'dragonruby-completion-disable)

(provide 'dragonruby-completion)
;;; dragonruby-completion.el ends here
