;;; dragonruby-sprites.el --- Visual previews for sprites -*- lexical-binding: t; -*-

(require 'dragonruby-project)
(require 'dragonruby-config)

(defconst dragonruby-supported-sprite-formats '("png" "bmp" "jpg" "jpeg" "gif"))
(defconst dragonruby-unsupported-sprite-formats '("webp" "svg" "psd" "tiff"))

;; Updated Regex: Supports "path" and 'path'
(defconst dragonruby--sprite-regex "[\"']\\([^\"]+\\.\\([a-zA-Z0-9]+\\)\\)[\"']")

(defun dragonruby--get-image-dimensions (path)
  "Get image dimensions (width x height) for PATH using ImageMagick or sips."
  (when (and path (file-exists-p path))
    (condition-case nil
        ;; Try using ImageMagick's identify first
        (let ((output (shell-command-to-string 
                       (format "identify -format '%%wx%%h' '%s' 2>/dev/null" path))))
          (if (string-match "\\([0-9]+\\)x\\([0-9]+\\)" output)
              (cons (string-to-number (match-string 1 output))
                    (string-to-number (match-string 2 output)))
            ;; Fallback to sips on macOS
            (let ((output (shell-command-to-string 
                          (format "sips -g pixelWidth -g pixelHeight '%s' 2>/dev/null | grep -E 'pixel(Width|Height)' | awk '{print $2}'" path))))
              (when (string-match "\\([0-9]+\\)\n\\([0-9]+\\)" output)
                (cons (string-to-number (match-string 1 output))
                      (string-to-number (match-string 2 output)))))))
      (error nil))))

(defun dragonruby--sprite-hover-info (path full-path)
  "Return rich tooltip with image preview and metadata."
  (when (and full-path (file-exists-p full-path))
    (let* ((dims (dragonruby--get-image-dimensions full-path))
           (width (and dims (car dims)))
           (height (and dims (cdr dims)))
           (ext (upcase (file-name-extension full-path)))
           (size-kb (/ (file-attribute-size (file-attributes full-path)) 1024))
           (img (create-image full-path nil nil :max-height 128 :max-width 128)))
      
      ;; Header: 64x64 PNG 12kb
      (concat
       (format "📏 %dx%d %s %dkb\n" (or width 0) (or height 0) ext size-kb)
       (propertize " " 'display img)
       (propertize "\n💡 Click to open" 'face 'italic)))))

(defun dragonruby--make-sprite-overlay (start end path full-path)
  "Create overlay with interactive sprite preview and rich metadata tooltip."
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'dragonruby-sprite-overlay t)
    
    ;; 1. Visual Hint (clickable path)
    (overlay-put ov 'face '(:underline t :foreground "cyan"))
    (overlay-put ov 'mouse-face '(:background "dark cyan" :foreground "white"))
    
    ;; 2. Hover: Show rich info with image preview
    (overlay-put ov 'help-echo
                 (lambda (_win _obj _pos)
                   (dragonruby--sprite-hover-info path full-path)))
    
    ;; 3. Click: Open File
    (overlay-put ov 'keymap
                 (let ((map (make-sparse-keymap)))
                   (define-key map [mouse-1]
                     (lambda () (interactive) (find-file full-path)))
                   map))
    ov))

(defun dragonruby--make-sprite-warning (start end msg color)
  "Create warning overlay for bad sprites."
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'dragonruby-sprite-overlay t)
    (overlay-put ov 'face `(:underline (:color ,color :style wave)))
    (overlay-put ov 'help-echo msg)
    (overlay-put ov 'mouse-face 'highlight)
    ov))

(defun dragonruby--scan-sprites-region (start end)
  "Scan region for sprite strings."
  (when dragonruby-enable-sprite-preview
    (save-excursion
      (goto-char start)
      (while (re-search-forward dragonruby--sprite-regex end t)
        (let* ((s (match-beginning 1))
               (e (match-end 1))
               (path (match-string 1))
               (ext (downcase (match-string 2)))
               (real-path (dragonruby--resolve-asset path)))
          
          (cond
           ((member ext dragonruby-supported-sprite-formats)
            (if real-path
                (dragonruby--make-sprite-overlay s e path real-path)
              (dragonruby--make-sprite-warning s e (format "Path not found: %s" path) "red")))
           
           ((member ext dragonruby-unsupported-sprite-formats)
            (dragonruby--make-sprite-warning s e (format "Format .%s not supported" ext) "orange"))))))))

(defun dragonruby--clear-sprite-overlays (start end)
  (remove-overlays start end 'dragonruby-sprite-overlay t))

(provide 'dragonruby-sprites)
