;;; dragonruby-assets.el --- Asset definitions and source finding w/ Smart Jump -*- lexical-binding: t; -*-

(require 'dragonruby-project)

(defconst dragonruby-asset-dirs
  '((sprite . "sprites")
    (background . "sprites")
    (audio . "audio")
    (font . "fonts")
    (code . "app")))

(defconst dragonruby-image-exts '("png" "bmp" "jpg" "jpeg" "gif")
  "Supported image extensions for DragonRuby.")
(defconst dragonruby-audio-exts '("wav" "ogg" "mp3")
  "Supported audio extensions for DragonRuby.")
(defconst dragonruby-font-exts  '("ttf" "otf")
  "Supported font extensions for DragonRuby.")
(defconst dragonruby-code-exts  '("rb")
  "Ruby source extensions for DragonRuby.")
(defconst dragonruby-data-exts  '("json" "txt" "csv" "tsv" "xml" "yml" "yaml")
  "Standard data and metadata extensions.")

;; Unify with legacy names for backward compatibility across all modules
(defvar dragonruby-supported-sprites dragonruby-image-exts)
(defvar dragonruby-unsupported-sprites '("svg" "psd" "xcf" "ase" "graphite"))

(defcustom dragonruby-sprite-source-extensions '(".ase" ".psd" ".xcf" ".graphite" ".kra")
  "List of source extensions to prioritize when opening a sprite."
  :type '(repeat string)
  :group 'dragonruby)

(defcustom dragonruby-experimental-smart-jump nil
  "Enable experimental Smart Source Jumping.
If non-nil, clicking a sprite or [Edit] opens the source file (e.g. .psd, .kra)
if found in the same directory or \\='art/\\=' folder."
  :type 'boolean
  :group 'dragonruby)

(defun dragonruby--find-source-file (path)
  "Find a source file (e.g. .psd, .kra, .xcf) for the given image PATH.
Checks:
1. The same directory as PATH.
2. The \\='art/\\=' directory at project root."
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

(defun dragonruby--collect-project-files (&optional type)
  "Return project files relative to root.
Strictly limits search directories based on TYPE:
- \\='ruby   : app/, lib/
- \\='data   : data/
- \\='sprite : sprites/
- \\='audio  : sounds/
- nil     : all standard directories."
  (let* ((root (dragonruby--find-project-root))
         (standard-dirs (pcase type
                          ('ruby   '("app" "lib" "data" "sprites" "audio" "fonts")) ; Universal Law
                          ('data   '("data"))
                          ('sprite '("sprites"))
                          ('audio  '("audio"))
                          ('font   '("fonts"))
                          (_       '("app" "data" "lib" "sprites" "audio" "fonts"))))
         (ruby-exts dragonruby-code-exts)
         (sprite-exts (if (boundp 'dragonruby-supported-sprites)
                          (append dragonruby-supported-sprites dragonruby-unsupported-sprites)
                        dragonruby-image-exts))
         (data-exts dragonruby-data-exts)
         (extensions (pcase type
                       ('ruby   (append ruby-exts sprite-exts data-exts dragonruby-audio-exts dragonruby-font-exts)) ; Law
                       ('data   data-exts)
                       ('sprite sprite-exts)
                       ('audio  dragonruby-audio-exts)
                       ('font   dragonruby-font-exts)
                       (_       (append ruby-exts sprite-exts data-exts dragonruby-audio-exts dragonruby-font-exts))))
         (pattern (concat "\\.\\(" (regexp-opt extensions) "\\)$")))
    (when (and root (file-directory-p root))
      (let (all-files)
        ;; 1. Collect files from specific subdirectories (recursively)
        (dolist (dir standard-dirs)
          (let ((full-dir (expand-file-name dir root)))
            (when (file-directory-p full-dir)
              (setq all-files (append all-files 
                                     (directory-files-recursively full-dir pattern))))))
        ;; 2. Always collect files from the root (main.rb, etc.)
        (setq all-files (append all-files 
                               (directory-files root t pattern t)))
        ;; Convert to relative paths and remove duplicates/directories
        (mapcar (lambda (f) (file-relative-name f root))
                (cl-remove-if #'file-directory-p (delete-dups all-files)))))))

(provide 'dragonruby-assets)
