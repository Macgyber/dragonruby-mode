;;; dragonruby-core.el --- Core aggregator -*- lexical-binding: t; -*-

(require 'dragonruby-utils)

;; Wrappers for potentially missing modules during restore


;; --- Minimal Registry Stub (to prevent void-function errors) ---
(defun dragonruby-registry-register (&rest _args)
  "Stub for removed registry functionality. Does nothing."
  nil)

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

;; --- Timer Configuration -------------------------------------------
;; These values document current hardcoded intervals.
;; Future: wire these into timer calls for user customization.

(defcustom dragonruby-popup-trigger-delay 0.2
  "Seconds to wait before showing sprite popup after hover."
  :type 'number
  :group 'dragonruby)

(defcustom dragonruby-popup-monitor-interval 0.25
  "Seconds between popup monitor checks."
  :type 'number
  :group 'dragonruby)

(defcustom dragonruby-scan-debounce-delay 0.6
  "Seconds to wait after typing before rescanning overlays."
  :type 'number
  :group 'dragonruby)

(provide 'dragonruby-core)
