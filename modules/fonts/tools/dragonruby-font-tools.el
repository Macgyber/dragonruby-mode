;;; dragonruby-font-tools.el --- Font Tools Entry Point -*- lexical-binding: t; -*-

(require 'dragonruby-kernel)
(require 'dragonruby-font-engine)
(require 'dragonruby-font-ui)

;; -----------------------------------------------------------------------------
;; 🧬 Lifecycle
;; -----------------------------------------------------------------------------

(defun dragonruby-font-tools-enable ()
  "Enable font tools."
  ;; Example: Add hooks if tools had background logic. 
  ;; Currently mainly a UI library loaded on demand.
  (message "🛠️ Font Tools Enabled"))

(defun dragonruby-font-tools-disable ()
  "Disable font tools."
  (message "🛠️ Font Tools Disabled"))

;; -----------------------------------------------------------------------------
;; 📜 Manifest
;; -----------------------------------------------------------------------------

(dragonruby-register-module
 :name 'font-tools
 :type :tool
 :namespace "dragonruby-font-" ;; Shares namespace with fonts for now
 :provides '(:font-manipulation)
 :requires '(:font-rendering)
 :entry-point 'dragonruby-font-tools
 :enable-fn #'dragonruby-font-tools-enable
 :disable-fn #'dragonruby-font-tools-disable)

(provide 'dragonruby-font-tools)
;;; dragonruby-font-tools.el ends here
