;;; dragonruby-font-overlay.el --- Visual previews for fonts -*- lexical-binding: t; -*-

(require 'dragonruby-core)
(require 'dragonruby-assets)

(defvar-local dragonruby--font-overlays nil
  "List of font overlays in the current buffer.")

(defvar dragonruby--font-scan-timer nil
  "Timer for debounced font scanning.")

(defun dragonruby--clear-font-overlays ()
  "Remove all font overlays from the current buffer."
  (remove-overlays (point-min) (point-max) 'dragonruby-font t)
  (setq dragonruby--font-overlays nil))

(defun dragonruby--make-font-preview (font-path)
  "Generate a preview image for the font at FONT-PATH using ImageMagick."
  (let* ((root (dragonruby--find-project-root))
         (bg-mode (frame-parameter nil 'background-mode))
         ;; Use project history or system temp as fallback
         (history-dir (if root 
                          (expand-file-name ".dr_history/font-tools" root)
                        (expand-file-name "dragonruby-mode/fonts" temporary-file-directory)))
         (bg-color (if (eq bg-mode 'dark) "#333333" "#E0E0E0"))
         (fg-color (if (eq bg-mode 'dark) "#FFFFFF" "#000000"))
         (mod-time (file-attribute-modification-time (file-attributes font-path)))
         (hash (md5 (format "%s-%s-%s" font-path mod-time bg-mode)))
         (preview-file (expand-file-name (concat hash ".png") history-dir)))
    
    (when (not (file-exists-p preview-file))
      (unless (file-directory-p history-dir) (make-directory history-dir t))
      (let ((cmd (if (executable-find "magick") "magick" "convert")))
        (call-process cmd nil nil nil
                      "-background" bg-color "-fill" fg-color
                      "-font" (expand-file-name font-path)
                      "-pointsize" "28"
                      "label:DragonRuby"
                      preview-file)))
    
    (when (file-exists-p preview-file)
      (create-image preview-file 'png nil :ascent 'center :scale 0.75))))

(defun dragonruby--make-font-overlay (start end path valid &optional unsupported)
  "Create a font overlay from START to END for PATH.
VALID indicates file exists. UNSUPPORTED indicates invalid format."
  (let* ((status (cond (unsupported 'unsupported)
                       (valid 'valid)
                       (t 'missing)))
         (face (pcase status
                 ('valid '(:underline (:color "#00FFFF" :style wave)))
                 ('unsupported '(:underline (:color "orange" :style wave)))
                 ('missing '(:underline (:color "red" :style wave)))))
         (help (pcase status
                 ('valid (format "Font: %s\n(RET to open viewer)" (file-name-nondirectory path)))
                 ('unsupported "❌ Format not supported by DragonRuby (Use TTF/OTF)")
                 ('missing "❌ Font not found")))
         (ov (make-overlay start end)))
    (overlay-put ov 'face face)
    (overlay-put ov 'help-echo help)
    (overlay-put ov 'dragonruby-font t)
    (when (eq status 'valid)
      ;; Inline Preview
      (let ((thumb (dragonruby--make-font-preview path)))
        (when thumb (overlay-put ov 'after-string (propertize " " 'display thumb))))
      
      ;; Interaction
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c C-o") (lambda () (interactive) (find-file path)))
        (define-key map (kbd "RET") (lambda () (interactive) (find-file path)))
        (overlay-put ov 'keymap map))
      (overlay-put ov 'mouse-face '(:background "#00FFFF" :foreground "black")))
    (overlay-put ov 'priority -40)
    (push ov dragonruby--font-overlays)))

(defun dragonruby--scan-font-overlays ()
  "Scan buffer for font references (ttf, otf, woff, etc.) inside strings."
  (when (and dragonruby-mode (bound-and-true-p dragonruby-font-mode))
    (dragonruby--clear-font-overlays)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        ;; Scan for common font extensions
        (while (re-search-forward "\"\\([^\"\n!]+\\.\\(ttf\\|otf\\|woff\\|woff2\\|svg\\|eot\\)\\)\"" nil t)
          (let* ((raw (match-string 1))
                 (start (match-beginning 0))
                 (end (match-end 0))
                 (ext (downcase (file-name-extension raw)))
                 (supported (member ext '("ttf" "otf")))
                 (abs (dragonruby--resolve-path raw 'font))
                 (valid (and supported (not (string-empty-p raw)) (file-exists-p abs))))
            (dragonruby--make-font-overlay start end abs valid (not supported))))))))

(defun dragonruby--refresh-font-overlays (&rest _)
  "Debounced refresh of font overlays."
  (when dragonruby--font-scan-timer
    (cancel-timer dragonruby--font-scan-timer))
  (setq dragonruby--font-scan-timer
        (run-with-idle-timer 0.6 nil
          (lambda (buf)
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (dragonruby--scan-font-overlays))))
          (current-buffer))))

(defun dragonruby--after-font-overlay-change (start end _len)
  "Trigger refresh after text change."
  (dragonruby--refresh-font-overlays))

(provide 'dragonruby-font-overlay)
;;; dragonruby-font-overlay.el ends here
