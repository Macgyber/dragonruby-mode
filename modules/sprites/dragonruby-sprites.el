;;; dragonruby-sprites.el --- Sprite previews and completion -*- lexical-binding: t; -*-

(require 'dragonruby-kernel)
(require 'cl-lib)
(require 'image-file)
(require 'dragonruby-core)

;; Load submodules (Moved to same directory)
(require 'dragonruby-sprite-model)
(require 'dragonruby-sprite-fs)
(require 'dragonruby-sprite-actions)
(require 'dragonruby-sprite-overlay)
(require 'dragonruby-sprite-popup)
(require 'dragonruby-sprite-hover)
(require 'dragonruby-sprite-completion)

;; 🧱 LEGACY MODE REMOVED
;; Use (dragonruby-enable 'sprites) instead.

;; -----------------------------------------------------------------------------
;; 🧬 Lifecycle
;; -----------------------------------------------------------------------------

(defun dragonruby-sprites-enable ()
  "Enable sprite services."
  (add-hook 'dragonruby-scan-hook #'dragonruby--scan-sprites nil t)
  (dragonruby--setup-capf)
  
  ;; Trigger initial scan if buffer is ready
  (when (eq major-mode 'ruby-mode)
    (dragonruby--scan-sprites))
    
  (message "🧱 Sprites Module Enabled"))

(defun dragonruby-sprites-disable ()
  "Disable sprite services."
  (remove-hook 'dragonruby-scan-hook #'dragonruby--scan-sprites t)
  (remove-hook 'completion-at-point-functions #'dragonruby-sprite-completion-at-point t)
  (remove-hook 'kill-buffer-hook #'dragonruby--sprite-popup-cleanup t)
  (dragonruby--sprite-popup-cleanup) ; Strict visual cleanup
  (dragonruby--clear-sprite-overlays)
  (message "🧱 Sprites Module Disabled"))

;; -----------------------------------------------------------------------------
;; 📜 Manifest
;; -----------------------------------------------------------------------------

(dragonruby-register-module
 :name 'sprites
 :type :main
 :namespace "dragonruby-sprite" ;; Matches file prefix
 :provides '(:rendering :atlases)
 :requires nil
 :entry-point 'dragonruby-sprites
 :enable-fn #'dragonruby-sprites-enable
 :disable-fn #'dragonruby-sprites-disable)

(provide 'dragonruby-sprites)
;;; dragonruby-sprites.el ends here
