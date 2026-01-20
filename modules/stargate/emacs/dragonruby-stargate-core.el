;;; dragonruby-stargate-core.el --- Stargate manifest for DragonRuby -*- lexical-binding: t -*-

;; Author: Macgyber <esteban3261g@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/Macgyber/dragonruby-mode

;;; Commentary:
;; Stargate: Time-traveling state management for DragonRuby (v1.0 Blindada).
;; Provides authoritative time-travel, branching, and state integrity.

;;; Code:

(require 'dragonruby-kernel)

;; Autoload implementation functions
(autoload 'dragonruby-stargate-enable "dragonruby-stargate-core--impl" nil t)
(autoload 'dragonruby-stargate-disable "dragonruby-stargate-core--impl" nil t)
(autoload 'dragonruby-stargate-inject-buffer "dragonruby-stargate-core--impl" nil t)
(autoload 'dragonruby-stargate-record "dragonruby-stargate-core--impl" nil t)
(autoload 'dragonruby-stargate-pause "dragonruby-stargate-core--impl" nil t)
(autoload 'dragonruby-stargate-jump "dragonruby-stargate-core--impl" nil t)

;; Autoload other public interface functions from sub-modules if needed
(autoload 'dragonruby-stargate-timeline "dragonruby-stargate-timeline" nil t)
(autoload 'dragonruby-stargate-session-init "dragonruby-stargate-manager" nil t)
(autoload 'dragonruby-stargate-session-load "dragonruby-stargate-manager" nil t)
(autoload 'dragonruby-stargate-session-fork "dragonruby-stargate-manager" nil t)

(defgroup dragonruby-stargate nil
  "Stargate: Time-traveling state management for DragonRuby."
  :group 'dragonruby)

;; --- KEYMAP (The Architect's Console) ---
(defvar dragonruby-stargate-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") #'dragonruby-stargate-record)
    (define-key map (kbd "p") #'dragonruby-stargate-pause)
    (define-key map (kbd "j") #'dragonruby-stargate-jump)
    (define-key map (kbd "t") #'dragonruby-stargate-timeline)
    (define-key map (kbd "l") #'dragonruby-stargate-session-load)
    map)
  "Keymap for Stargate commands.")

;; Global Shortcuts
(global-set-key (kbd "<f7>") #'dragonruby-stargate-record)
(global-set-key (kbd "<f8>") #'dragonruby-stargate-pause)
(global-set-key (kbd "<f9>") #'dragonruby-stargate-timeline)

;; -----------------------------------------------------------------------------
;; 📜 Manifest
;; -----------------------------------------------------------------------------

(dragonruby-register-module
 :name 'stargate
 :type 'feature
 :namespace "dragonruby-stargate"
 :provides '(time-travel branching state-integrity)
 :requires '(:core :scheduler)
 :entry-point 'dragonruby-stargate-core--impl
 :enable-fn #'dragonruby-stargate-enable
 :disable-fn #'dragonruby-stargate-disable)

;; --- ALIASES (The Architect's Shortcuts) ---
(defalias 'stargate-init #'dragonruby-stargate-session-init)
(defalias 'stargate-timeline #'dragonruby-stargate-timeline)
(defalias 'stargate-load #'dragonruby-stargate-session-load)
(defalias 'stargate-fork #'dragonruby-stargate-session-fork)

(provide 'dragonruby-stargate-core)
;;; core.el ends here
