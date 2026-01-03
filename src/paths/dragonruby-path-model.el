;;; dragonruby-path-model.el --- Path extensions and configuration -*- lexical-binding: t; -*-

(require 'dragonruby-assets)
(require 'dragonruby-registry)

;; Legacy Aliases
(defvar dragonruby-data-extensions dragonruby-data-exts)
(defvar dragonruby-ruby-extensions dragonruby-code-exts)

;; Register Path-specific snippets
(dragonruby-registry-register 'paths
  '(:snippets (("req" . "require \"\"")
               ("reqr" . "require_relative \"\"")
               ("load" . "load \"\"")
               ("read" . "$gtk.read_file \"\"")
               ("json" . "$gtk.parse_json_file \"\"")
               ("script" . "load_script \"\""))))

(provide 'dragonruby-path-model)
;;; dragonruby-path-model.el ends here
