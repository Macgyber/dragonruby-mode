;;; dragonruby-audio.el --- Audio services for DragonRuby -*- lexical-binding: t; -*-

(require 'dragonruby-kernel)
(require 'dragonruby-core)
(require 'dragonruby-audio-model)
(require 'dragonruby-audio-fs)
(require 'dragonruby-audio-overlay)

;; -----------------------------------------------------------------------------
;; 🧬 Lifecycle
;; -----------------------------------------------------------------------------

(defun dragonruby-audio-enable ()
  "Enable audio services."
  (add-hook 'dragonruby-scan-hook #'dragonruby--scan-audio-overlays nil t)
  (message "🔊 Audio Module Enabled"))

(defun dragonruby-audio-disable ()
  "Disable audio services."
  (remove-hook 'dragonruby-scan-hook #'dragonruby--scan-audio-overlays t)
  (dragonruby--clear-audio-overlays)
  (message "🔊 Audio Module Disabled"))

;; -----------------------------------------------------------------------------
;; 📜 Manifest
;; -----------------------------------------------------------------------------

(dragonruby-register-module
 :name 'audio
 :type :main
 :namespace "dragonruby-audio-"
 :provides '(:visuals :audio-detection)
 :requires nil
 :entry-point 'dragonruby-audio
 :enable-fn #'dragonruby-audio-enable
 :disable-fn #'dragonruby-audio-disable)

(provide 'dragonruby-audio)
;;; dragonruby-audio.el ends here
