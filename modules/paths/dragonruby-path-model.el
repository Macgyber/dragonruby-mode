;;; dragonruby-path-model.el --- Path extensions and configuration -*- lexical-binding: t; -*-

(require 'dragonruby-core)


;; Legacy Aliases
(defvar dragonruby-data-extensions dragonruby-data-exts)
(defvar dragonruby-ruby-extensions dragonruby-code-exts)

;; Path-specific snippets
(defconst dragonruby-path-snippets
  '(("req" . "require \"\"")
    ("reqr" . "require_relative \"\"")
    ("load" . "load \"\"")
    ("read" . "$gtk.read_file \"\"")
    ("json" . "$gtk.parse_json_file \"\"")
    ("script" . "load_script \"\"")))

(provide 'dragonruby-path-model)
;;; dragonruby-path-model.el ends here
