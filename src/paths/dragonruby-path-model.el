;;; dragonruby-path-model.el --- Path extensions and configuration -*- lexical-binding: t; -*-

;; Pure data - no side effects

(defvar dragonruby-data-extensions
  '("json" "txt" "csv" "tsv" "xml" "yml" "yaml"
    "png" "jpg" "jpeg" "gif" "bmp" "ogg" "wav" "mp3")
  "Supported data and asset file extensions for DragonRuby.")

(defvar dragonruby-ruby-extensions
  '("rb")
  "Ruby file extensions.")

(defvar dragonruby--require-snippets
  '(("req" . "require \"\"")
    ("reqr" . "require_relative \"\"")
    ("load" . "load \"\"")
    ("read" . "$gtk.read_file \"\"")
    ("json" . "$gtk.parse_json_file \"\"")
    ("script" . "load_script \"\"")
    ("spr" . "\"sprites/.png\""))
  "Snippets for quick path insertion.")

(provide 'dragonruby-path-model)
;;; dragonruby-path-model.el ends here
