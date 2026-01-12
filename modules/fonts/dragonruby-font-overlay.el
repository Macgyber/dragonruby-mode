;;; dragonruby-font-overlay.el --- Visual previews for fonts -*- lexical-binding: t; -*-

(require 'dragonruby-core)

(defvar-local dragonruby--font-overlays nil
  "List of font overlays in the current buffer.")

(defconst dragonruby--font-regex "['\"’“”]\\([^'\"\n!’“”]+\\.\\(ttf\\|otf\\|woff\\|woff2\\|svg\\|eot\\)\\)['\"’“”]"
  "Regex to find font references.")


(defun dragonruby--clear-font-overlays ()
  "Remove all font overlays from the current buffer."
  (remove-overlays (point-min) (point-max) 'dragonruby-font t)
  (setq dragonruby--font-overlays nil))

(defcustom dragonruby-font-preview-text "DragonRuby"
  "Default text to show in inline font preview.
Users can customize this to their game's text (e.g., 'Press Start')."
  :type 'string
  :group 'dragonruby)

(defun dragonruby--make-font-preview (font-path &optional context-text)
  "Generate a preview for the font at FONT-PATH.
Uses CONTEXT-TEXT if provided (detected from nearby text: in code), 
otherwise falls back to `dragonruby-font-preview-text`.
Uses ImageMagick if available, otherwise falls back to emoji icon."
  ;; Check if ImageMagick is available
  ;; Check if ImageMagick is available
  (if (not (or (executable-find "magick") (executable-find "convert")))
      ;; FALLBACK: No ImageMagick - show emoji icon
      (propertize " 🔤" 'face '(:foreground "#00FFFF" :height 0.9)
                  'help-echo "ImageMagick not found - Install to see font preview")
    ;; ImageMagick available - render preview
    (let* ((root (dragonruby--find-project-root))
           (bg-mode (frame-parameter nil 'background-mode))
           ;; Use system temp cache (not project folder)
           (history-dir (dragonruby--cache-dir "fonts"))
           (bg-color (if (eq bg-mode 'dark) "#333333" "#E0E0E0"))
           (fg-color (if (eq bg-mode 'dark) "#FFFFFF" "#000000"))
           ;; Use context text if provided, otherwise global default
           ;; SAFETY: Ensure we have a valid string to avoid length crash
           (raw-text (or context-text dragonruby-font-preview-text "Aa"))
           ;; Truncate to 10 chars for inline preview
           (preview-text (if (> (length raw-text) 10)
                             (concat (substring raw-text 0 10) "…")
                           raw-text))
           (mod-time (let ((attrs (file-attributes font-path)))
                       (if attrs (file-attribute-modification-time attrs) "0")))
           (hash (md5 (format "%s-%s-%s-%s" font-path mod-time bg-mode preview-text)))
           (preview-file (expand-file-name (concat hash ".png") history-dir)))
      
      ;; RE-GENERATION LOGIC:
      ;; Generate if:
      ;; 1. File doesn't exist
      ;; 2. File exists but is empty (0 bytes - corruption artifact)
      (when (or (not (file-exists-p preview-file))
                (and (file-exists-p preview-file)
                     (= (file-attribute-size (file-attributes preview-file)) 0)))
        
        ;; Clean up bad file if it exists
        (when (file-exists-p preview-file) (delete-file preview-file))
        
        (unless (file-directory-p history-dir) (make-directory history-dir t))
        (let ((cmd (if (executable-find "magick") "magick" "convert")))
          (ignore-errors
            (call-process cmd nil nil nil
                          "-background" bg-color "-fill" fg-color
                          "-font" (expand-file-name font-path)
                          "-pointsize" "28"
                          (format "label:%s" preview-text)
                          preview-file))))
      
      (let* ((valid-img (and (file-exists-p preview-file)
                             (> (file-attribute-size (file-attributes preview-file)) 0)))
             (image (when valid-img
                      (let ((inhibit-message t))
                        (ignore-errors
                          (create-image preview-file 'png nil :ascent 'center :scale 0.75))))))
        (or image
            ;; If rendering failed, show fallback
            (propertize " 🔤" 'face '(:foreground "#FF8C00" :height 0.9)
                        'help-echo "Preview Generation Failed"))))))

(defun dragonruby--make-font-overlay (start end path valid &optional unsupported context-text)
  "Create a font overlay from START to END for PATH.
VALID indicates file exists. UNSUPPORTED indicates invalid format.
CONTEXT-TEXT is the nearby text: value to show in preview."
  (let* ((status (cond (unsupported 'unsupported)
                       (valid 'valid)
                       (t 'missing)))
         (help (pcase status
                 ('valid (format "Font: %s\n(RET to open viewer)" (file-name-nondirectory path)))
                 ('unsupported "❌ Format not supported by DragonRuby (Use TTF/OTF)")
                 ('missing "❌ Font not found")))
         (ov (make-overlay start end)))
    ;; SIMPLIFIED VISUAL LANGUAGE:
    ;; - Valid: NO underline, only mini-preview icon
    ;; - Invalid: Wavy underline (error indicator)
    (unless (eq status 'valid)
      (let ((face (pcase status
                    ('unsupported '(:underline (:color "orange" :style wave)))
                    ('missing '(:underline (:color "red" :style wave))))))
        (overlay-put ov 'face face)))
    (overlay-put ov 'help-echo (or help ""))
    (overlay-put ov 'dragonruby-font t)
    (when (eq status 'valid)
      ;; Inline Preview (with context text if available)
      (let ((thumb (dragonruby--make-font-preview path context-text)))
        (when thumb (overlay-put ov 'after-string (propertize " " 'display thumb))))
      
      ;; Interaction
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c C-o") (lambda () (interactive) (find-file path)))
        (define-key map (kbd "RET") (lambda () (interactive) (find-file path)))
        (overlay-put ov 'keymap map))
      (overlay-put ov 'mouse-face '(:background "#00FFFF" :foreground "black")))
    (overlay-put ov 'priority -40)
    (push ov dragonruby--font-overlays)))

(defun dragonruby--find-nearby-text (pos)
  "Find nearby text: value around POS for font context preview.
Searches backwards first, then forwards. Matches various Ruby hash/label styles."
  (save-excursion
    (goto-char pos)
    (let* ((limit-back (max (point-min) (- pos 350)))
           (limit-fwd (min (point-max) (+ pos 150)))
           ;; Regex that specifically looks for text keywords followed by a value
           (regex "\\b\\(?:text\\|label\\|string\\)[ \t]*[:=]>*[ \t]*['\"’“”]\\([^'\"\n’“”]+\\)['\"’“”]")
           (found nil))
      
      ;; 1. Search BACKWARDS
      (goto-char pos)
      (while (and (not found) (re-search-backward regex limit-back t))
        (let ((match (match-string 1)))
          ;; Ensure we didn't just match the font path itself
          (unless (string-match-p "\\.\\(ttf\\|otf\\|otf\\|woff\\)$" match)
            (setq found match))))
      
      ;; 2. Search FORWARDS (if not found backwards)
      (unless found
        (goto-char pos)
        (while (and (not found) (re-search-forward regex limit-fwd t))
          (let ((match (match-string 1)))
            (unless (string-match-p "\\.\\(ttf\\|otf\\|otf\\|woff\\)$" match)
              (setq found match)))))
      found)))

(defun dragonruby--scan-font-overlays ()
  "Scan VISIBLE region for font references.
Adheres to the Constitutional Principle: Only work what is seen."
  (when dragonruby-mode
    (let* ((region (dragonruby--visible-region))
           (start-pos (car region))
           (end-pos (cdr region)))
      ;; 1. Clear overlays ONLY in the region we are about to scan
      (remove-overlays start-pos end-pos 'dragonruby-font t)
      
      (save-excursion
        (goto-char start-pos)
        (while (re-search-forward dragonruby--font-regex end-pos t)
          (let* ((raw (match-string 1))
                 (start (match-beginning 1))
                 (end (match-end 1))
                 (ext (downcase (or (file-name-extension raw) "")))
                 (supported (member ext '("ttf" "otf")))
                 (abs (dragonruby--resolve-path raw 'font))
                 (exists (and abs (file-exists-p abs)))
                 (valid (and supported (not (string-empty-p raw)) exists))
                 (context-text (dragonruby--find-nearby-text start)))
            (dragonruby--make-font-overlay start end abs valid (not supported) (or context-text ""))))))))


(provide 'dragonruby-font-overlay)
;;; dragonruby-font-overlay.el ends here
