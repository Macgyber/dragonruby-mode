;;; dragonruby-stargate-core.el --- Stargate manifest for DragonRuby -*- lexical-binding: t -*-

(require 'dragonruby-kernel)

;; Autoload implementation functions
(autoload 'dragonruby-stargate-enable "dragonruby-stargate-core--impl" nil t)
(autoload 'dragonruby-stargate-disable "dragonruby-stargate-core--impl" nil t)
(autoload 'dragonruby-stargate-inject-buffer "dragonruby-stargate-core--impl" nil t)

;; Autoload other public interface functions from sub-modules if needed
(autoload 'dragonruby-stargate-timeline "dragonruby-stargate-timeline" nil t)
(autoload 'dragonruby-stargate-session-init "dragonruby-stargate-manager" nil t)

(defgroup dragonruby-stargate nil
  "Stargate: Time-traveling state management for DragonRuby."
  :group 'dragonruby)

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

(provide 'dragonruby-stargate-core)
;;; core.el ends here
