;;; dragonruby-core.el --- Core aggregator and constants -*- lexical-binding: t; -*-

;; Protection against re-loading core logic accidentally
(defvar dragonruby--core-loaded nil)

(unless dragonruby--core-loaded
  (setq dragonruby--core-loaded t)

  (require 'dragonruby-utils)
  (require 'dragonruby-knowledge)

  ;; 🛠️ UX Foundation: Central Customization Group
  (defgroup dragonruby nil
    "DragonRuby Mode core configuration."
    :group 'tools)

  ;; --- Global Extension Lists ---
  (defconst dragonruby-data-exts '("txt" "json" "csv" "xml" "yml" "yaml")
    "Extensions considered as data files.")

  (defconst dragonruby-code-exts '("rb")
    "Extensions considered as ruby code.")

  (defconst dragonruby-image-exts '("png" "jpg" "jpeg" "gif" "bmp" "webp" "svg")
    "Extensions considered as image files.")

  (defconst dragonruby-audio-exts '("wav" "ogg")
    "Extensions considered as audio files.")

  (defconst dragonruby-font-exts '("ttf" "otf")
    "Extensions considered as font files.")

  ;; --- Minimal Registry Stub (Legacy Bridge) ---
  (defun dragonruby-registry-register (&rest _args)
    "DEPRECATED: Use (dragonruby-register-module ...) through the kernel."
    (error "dragonruby-registry-register is deprecated. Use dragonruby-register-module via kernel."))

  ;; Note: Timing and latency variables (delays, intervals) 
  ;; have been moved to dragonruby-scheduler.el (the Heart/Nervous System).
  )

(provide 'dragonruby-core)
