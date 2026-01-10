;;; dragonruby-path-overlay.el --- Visual overlays for paths -*- lexical-binding: t; -*-

(require 'dragonruby-core)
(require 'dragonruby-path-model)
(require 'dragonruby-path-fs)

(defvar-local dragonruby--path-overlays nil
  "List of path overlays in the current buffer.")

(defun dragonruby--clear-path-overlays ()
  "Clear all path overlays using property-based cleanup."
  (remove-overlays (point-min) (point-max) 'dragonruby-path t)
  (setq dragonruby--path-overlays nil))

(defun dragonruby--make-path-overlay (start end abs-path valid)
  "Create overlay from START to END for ABS-PATH.
VALID indicates if file exists."
  (let* ((exists (and abs-path (file-exists-p abs-path)))
         ;; SIMPLIFIED VISUAL LANGUAGE:
         ;; - Valid: Color only (no underline) - less visual noise
         ;; - Invalid: Wavy red underline (error indicator)
         (face (cond (exists '(:foreground "#2196f3" :weight bold))
                     ((null abs-path) nil)
                     (t '(:underline (:style wave :color "red")))))
         (help (cond (exists (format "C-c C-o to open: %s" (file-relative-name abs-path)))
                     ((null abs-path) "⚠️ Context unknown")
                     (t "❌ File not found"))))
    (let ((ov (make-overlay start end)))
      (overlay-put ov 'face face)
      (overlay-put ov 'help-echo help)
      (overlay-put ov 'dragonruby-path t)
      (when valid
        (overlay-put ov 'keymap
                     (let ((map (make-sparse-keymap)))
                       (define-key map (kbd "C-c C-o")
                         (lambda () (interactive) (find-file abs-path)))
                       (define-key map (kbd "RET")
                         (lambda () (interactive) (find-file abs-path)))
                       map))
        (overlay-put ov 'mouse-face '(:background "#2ECC71" :foreground "black")))
      (overlay-put ov 'priority -50)
      (push ov dragonruby--path-overlays))))

(defun dragonruby--scan-paths ()
  "Scan buffer for require/load statements and data file paths.
Designed for high-frequency execution during typing."
  (dragonruby--clear-path-overlays)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      ;; Ruby requires (require, require_relative, load)
      (while (re-search-forward
              "\\(require\\|require_relative\\|load\\)\\s-*[( ]\\s-*['\"]\\([^'\"]*\\)['\"]"
              nil t)
        (let* ((cmd (match-string 1))
               (raw (match-string 2))
               (start (match-beginning 0))
               (end (match-end 0))
               (type (intern cmd))
               (abs (dragonruby--resolve-path raw type))
               ;; Empty paths are never valid
               (valid (and (not (string-empty-p raw)) abs (file-exists-p abs))))
          (dragonruby--make-path-overlay start end abs valid)))
      
      ;; DragonRuby Data Reading (read_file, parse_json_file, etc)
      (goto-char (point-min))
      (while (re-search-forward
              "\\(read_file\\|parse_json_file\\|parse_json\\|load_script\\)\\s-*[( ]\\s-*['\"]\\([^'\"]*\\)['\"]"
              nil t)
        (let* ((cmd (match-string 1))
               (raw (match-string 2))
               (start (match-beginning 0))
               (end (match-end 0))
               (type (if (string= cmd "load_script") 'ruby 'data))
               (abs (dragonruby--resolve-path raw type))
               (valid (and (not (string-empty-p raw)) abs (file-exists-p abs))))
          (dragonruby--make-path-overlay start end abs valid)))
          
      ;; Generic Data files inside strings (extension-based)
      (goto-char (point-min))
      (while (re-search-forward "\"\\([^\"\n!]+\\.[a-z0-9]+\\)\"" nil t)
        (let* ((raw (match-string 1))
               (ext (downcase (or (file-name-extension raw) "")))
               (start (match-beginning 0))
               (end (match-end 0)))
          (cond
           ;; Image Files (The new Sprite Law)
           ((member ext dragonruby-image-exts)
            (let* ((abs (dragonruby--resolve-path raw 'sprite))
                   (exists (and abs (file-exists-p abs))))
              (dragonruby--make-path-overlay start end abs exists)))
           ;; Data Files
           ((member ext dragonruby-data-exts)
            (let* ((abs (dragonruby--resolve-path raw 'data))
                   (exists (and abs (file-exists-p abs))))
              (dragonruby--make-path-overlay start end abs exists)))))))))

(provide 'dragonruby-path-overlay)
;;; dragonruby-path-overlay.el ends here
