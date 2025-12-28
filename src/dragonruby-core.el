;;; dragonruby-core.el --- Project detection and file utilities -*- lexical-binding: t; -*-

(defconst dragonruby-asset-dirs
  '((sprite . "sprites")
    (background . "sprites")
    (audio . "audio")
    (code . "app")))

(defconst dragonruby-image-exts '("png" "bmp" "jpg" "jpeg"))
(defconst dragonruby-audio-exts '("wav" "ogg" "mp3"))
(defconst dragonruby-code-exts  '("rb"))

(defun dragonruby--find-project-root ()
  "Find the root of the DragonRuby project.
Looks for app/main.rb, dragonruby executable, or .dragonruby/ folder."
  (let ((dir (file-name-directory (or buffer-file-name default-directory))))
    (or (locate-dominating-file dir "app/main.rb")
        (locate-dominating-file dir "dragonruby")
        (locate-dominating-file dir ".dragonruby/")
        (locate-dominating-file dir "app")
        dir)))

;; Alias for backward compatibility
(defalias 'dragonruby--project-root 'dragonruby--find-project-root)

(defun dragonruby--files-in (dir extensions)
  "List files recursively in DIR matching EXTENSIONS."
  (when (and dir (file-directory-p dir))
    (directory-files-recursively
     dir
     (concat "\\." (regexp-opt extensions) "$"))))

;; --- DEBOUNCE UTILITY ---
(defvar-local dragonruby--debounce-timer nil
  "Timer for debouncing scan operations.")

(defun dragonruby--debounce (func delay)
  "Run FUNC after DELAY seconds, canceling any pending call."
  (when dragonruby--debounce-timer
    (cancel-timer dragonruby--debounce-timer))
  (setq dragonruby--debounce-timer
        (run-with-idle-timer delay nil func)))

(provide 'dragonruby-core)

