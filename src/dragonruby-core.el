;;; dragonruby-core.el --- Project detection and file utilities -*- lexical-binding: t; -*-

(defconst dragonruby-asset-dirs
  '((sprite . "sprites")
    (background . "sprites")
    (audio . "audio")
    (code . "app")))

(defconst dragonruby-image-exts '("png" "bmp" "jpg" "jpeg"))
(defconst dragonruby-audio-exts '("wav" "ogg" "mp3"))
(defconst dragonruby-code-exts  '("rb"))

(defcustom dragonruby-sprite-source-extensions '(".aseprite" ".ase" ".psd" ".xcf" ".graphite")
  "List of source extensions to prioritize when opening a sprite."
  :type '(repeat string)
  :group 'dragonruby)

(defcustom dragonruby-experimental-smart-jump nil
  "Enable experimental Smart Source Jumping.
If non-nil, clicking a sprite or [Edit] opens the source file (e.g. .aseprite)
if found in the same directory or 'art/' folder."
  :type 'boolean
  :group 'dragonruby)

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

(defun dragonruby--find-source-file (path)
  "Find a source file (e.g. .aseprite) for the given image PATH.
Checks:
1. The same directory as PATH.
2. The 'art/' directory at project root."
  (let ((base-name (file-name-sans-extension path))
        (extensions dragonruby-sprite-source-extensions)
        (root (dragonruby--find-project-root))
        (found nil))
    
    ;; 1. Check local directory
    (dolist (ext extensions)
      (let ((source-path (concat base-name ext)))
        (when (and (not found) (file-exists-p source-path))
          (setq found source-path))))
    
    ;; 2. Check 'art/' folder if not found locally
    (when (and (not found) root)
      (let* ((rel-name (file-name-nondirectory base-name))
             (art-dir (expand-file-name "art" root)))
        (when (file-directory-p art-dir)
          (dolist (ext extensions)
            (let ((source-path (expand-file-name (concat rel-name ext) art-dir)))
              (when (and (not found) (file-exists-p source-path))
                (setq found source-path)))))))
    found))

(provide 'dragonruby-core)

