;;; dragonruby-project.el --- Project detection and file utilities -*- lexical-binding: t; -*-

(defconst dragonruby-asset-dirs
  '((sprite . "sprites")
    (background . "sprites")
    (audio . "audio")
    (code . "app")))

(defconst dragonruby-image-exts '("png" "bmp" "jpg" "jpeg"))
(defconst dragonruby-audio-exts '("wav" "ogg" "mp3"))
(defconst dragonruby-code-exts  '("rb"))

(defun dragonruby--project-root ()
  "Detect the root of the DragonRuby project (looks for 'app' folder)."
  (locate-dominating-file default-directory "app"))

(defun dragonruby--files-in (dir extensions)
  "List files recursively in DIR matching EXTENSIONS."
  (when (and dir (file-directory-p dir))
    (directory-files-recursively
     dir
     (concat "\\." (regexp-opt extensions) "$"))))

  (let ((root (dragonruby--project-root)))
    (when root
      (let* ((rb-path (if (string-suffix-p ".rb" path) path (concat path ".rb")))
             (full-path (expand-file-name rb-path root)))
        full-path))))

(defun dragonruby--resolve-asset (path)
  "Check if an asset exists relative to project root (or current dir)."
  (let ((root (or (dragonruby--project-root) default-directory))) ;; Fallback to current dir
    (when root
      (let ((full-path (expand-file-name path root)))
        (if (file-exists-p full-path)
            full-path
          ;; Try direct relative check as last resort
          (if (file-exists-p path)
              (expand-file-name path)
            nil))))))

(provide 'dragonruby-project)
