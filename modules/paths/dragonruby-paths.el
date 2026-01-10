;;; dragonruby-paths.el --- Universal Code & Data Navigation (Facade) -*- lexical-binding: t; -*-

(require 'dragonruby-kernel)
(require 'dragonruby-core)
(require 'dragonruby-path-model)
(require 'dragonruby-path-fs)
(require 'dragonruby-path-snippets)
(require 'dragonruby-path-overlay)
(require 'dragonruby-path-actions)
(require 'dragonruby-path-completion)

(defvar dragonruby-enable-path-completion t)

(defvar dragonruby-paths-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c o") #'dragonruby-open-project-path)
    (define-key map (kbd "C-M-i") #'dragonruby-smart-complete)
    map)
  "Keymap for dragonruby-paths module.")

(define-minor-mode dragonruby-paths-mode
  "Local minor mode for DragonRuby paths."
  :lighter ""
  :keymap dragonruby-paths-mode-map)

(defun dragonruby--paths-hook ()
  (dragonruby-paths-mode 1))

;; -----------------------------------------------------------------------------
;; 🧬 Lifecycle
;; -----------------------------------------------------------------------------

(defun dragonruby-paths-enable ()
  "Enable path services."
  (add-hook 'dragonruby-scan-hook #'dragonruby--scan-paths nil t)
  ;; Hook keymap activation
  (add-hook 'ruby-mode-hook #'dragonruby--paths-hook)
  (when (eq major-mode 'ruby-mode) (dragonruby-paths-mode 1))
  
  (when dragonruby-enable-path-completion
    (add-hook 'completion-at-point-functions #'dragonruby-path-completion-at-point 90 t))
    
  (message "📂 Paths Module Enabled"))

(defun dragonruby-paths-disable ()
  "Disable path services."
  (remove-hook 'dragonruby-scan-hook #'dragonruby--scan-paths t)
  (remove-hook 'ruby-mode-hook #'dragonruby--paths-hook)
  (remove-hook 'completion-at-point-functions #'dragonruby-path-completion-at-point t)
  (dragonruby--clear-path-overlays)
  (dragonruby-paths-mode -1)
  (message "📂 Paths Module Disabled"))

;; -----------------------------------------------------------------------------
;; 📜 Manifest
;; -----------------------------------------------------------------------------

(dragonruby-register-module
 :name 'paths
 :type :main
 :namespace "dragonruby-path" ;; Loose match for dragonruby-paths-mode/map too
 :provides '(:navigation :code-intelligence)
 :requires nil
 :entry-point 'dragonruby-paths
 :enable-fn #'dragonruby-paths-enable
 :disable-fn #'dragonruby-paths-disable)

(provide 'dragonruby-paths)
;;; dragonruby-paths.el ends here
