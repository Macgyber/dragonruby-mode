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

(defun dragonruby--scan-paths (&optional beg end)
  "Scan VISIBLE region for require/load statements and data file paths.
Designed for high-frequency execution during the Kernel pulse."
  (when dragonruby-mode
    (let* ((start-pos (or beg (point-min)))
           (end-pos (or end (point-max))))
      
      ;; 1. Clear overlays only in the scanned region
      (remove-overlays start-pos end-pos 'dragonruby-path t)
      
      (save-excursion
        (goto-char start-pos)
        ;; Path Regex: require/load/read_file/parse_json
        (while (re-search-forward
                "\\(require\\|require_relative\\|load\\|read_file\\|parse_json_file\\|parse_json\\|load_script\\)\\s-*[( ]\\s-*['\"]\\([^'\"]*\\)['\"]"
                end-pos t)
          (let* ((cmd (match-string 1))
                 (raw (match-string 2))
                 (start (match-beginning 0))
                 (end (match-end 0))
                 (type (cond ((string= cmd "require_relative") 'require_relative)
                             ((member cmd '("require" "load" "load_script")) 'ruby)
                             (t 'data)))
                 (abs (dragonruby--resolve-path raw type))
                 ;; Only work if it's a valid string
                 (valid (and (not (string-empty-p raw)) abs)))
            (dragonruby--make-path-overlay start end abs valid)))

        ;; Scan for extension-based data files (txt, json, etc)
        ;; excluding sprites/fonts to avoid double-overlays
        (goto-char start-pos)
        (while (re-search-forward "\"\\([^\"\n!]+\\.\\(txt\\|json\\|csv\\|xml\\|yml\\|yaml\\)\\)\"" end-pos t)
          (let* ((raw (match-string 1))
                 (start (match-beginning 1))
                 (end (match-end 1))
                 (abs (dragonruby--resolve-path raw 'data)))
            (when (and abs (not (string-empty-p raw)))
              (dragonruby--make-path-overlay start end abs t))))))))

(provide 'dragonruby-path-overlay)
;;; dragonruby-path-overlay.el ends here
