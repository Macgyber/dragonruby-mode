;;; dragonruby-sprites.el --- Sprite previews (Inline + Hover) -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'image-file)

(defvar dragonruby--sprite-overlays nil)
(defvar dragonruby-supported-sprites '("png" "jpg" "jpeg" "gif" "bmp"))
(defvar dragonruby-unsupported-sprites '("svg" "psd" "xcf" "ase" "aseprite"))

;; --- PATH RESOLUTION ---
(defun dragonruby--find-project-root ()
  (let ((dir (file-name-directory (or buffer-file-name default-directory))))
    (or (locate-dominating-file dir "app/main.rb")
        (locate-dominating-file dir ".dragonruby/")
        dir)))

(defun dragonruby--resolve-asset-path (path)
  (let ((root (dragonruby--find-project-root)))
    (expand-file-name path root)))

;; --- AUTOCOMPLETE (CAPF) ---
(defun dragonruby--get-all-sprites-in-project ()
  (let* ((root (dragonruby--find-project-root))
         (files (directory-files-recursively root "\\.\\(png\\|jpg\\|jpeg\\|gif\\|bmp\\)$")))
    (mapcar (lambda (f) (file-relative-name f root)) files)))

(defun dragonruby-sprite-completion-at-point ()
  (let ((state (syntax-ppss)))
    (when (nth 3 state)
      (save-excursion
        (let* ((start (nth 8 state))
               (end (point))
               (str-content (buffer-substring-no-properties (1+ start) end))
               (is-path (or (string-match-p "/" str-content)
                            (string-match-p "\\.\\(png\\|jpg\\)$" str-content)
                            (> (length str-content) 0)))) 
          (when is-path
            (list (1+ start) (nth 1 (syntax-ppss))
                  (dragonruby--get-all-sprites-in-project)
                  :exclusive 'no)))))))

;; --- RICH VISUALS (INLINE + HOVER) ---
(defun dragonruby--create-tooltip-string (path)
  "Create a large preview for hover."
  (if (file-exists-p path)
      (let* ((image (create-image path nil nil :max-width 300 :max-height 300))
             (attrs (file-attributes path))
             (size (file-size-human-readable (file-attribute-size attrs)))
             (info-text (format " \n📦 %s\n📏 %s" (upcase (file-name-extension path)) size)))
        (concat (propertize " " 'display image) info-text))
    "❌ Text file not found"))

(defun dragonruby--create-inline-thumb (path)
  "Create a tiny 20px thumbnail for inline display."
  (when (file-exists-p path)
    (let ((image (create-image path nil nil :height 20 :ascent 'center)))
      (concat (propertize " " 'display '(space :width 1)) ;; Margin
              (propertize " " 'display image)))))

(defun dragonruby--make-sprite-overlay (start end text path status)
  (let ((ov (make-overlay start end))
        (color (pcase status ('valid "cyan") ('missing "red") ('unsupported "orange")))
        (style (if (eq status 'valid) nil 'wave))
        ;; 1. HOVER TOOLTIP (Large)
        (tooltip (if path (dragonruby--create-tooltip-string path) "Missing")))
    
    (overlay-put ov 'face `(:underline (:color ,color :style ,style)))
    (overlay-put ov 'help-echo tooltip)
    
    ;; 2. INLINE THUMBNAIL (Tiny)
    (when (eq status 'valid)
      (let ((thumb (dragonruby--create-inline-thumb path)))
        (when thumb
          (overlay-put ov 'after-string thumb))))
    
    (when (eq status 'valid)
      (overlay-put ov 'keymap 
                   (let ((map (make-sparse-keymap)))
                     (define-key map [mouse-1] (lambda () (interactive) (find-file path)))
                     map))
      (overlay-put ov 'mouse-face 'highlight))
    (push ov dragonruby--sprite-overlays)))

(defun dragonruby--scan-sprites ()
  (mapc #'delete-overlay dragonruby--sprite-overlays)
  (setq dragonruby--sprite-overlays nil)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\"\\([^\"\n]+\\)\"" nil t)
      (let* ((start (match-beginning 1))
             (end (match-end 1))
             (raw-path (match-string 1))
             (ext (file-name-extension raw-path)))
        (when ext
          (setq ext (downcase ext))
          (cond
           ((member ext dragonruby-supported-sprites)
            (let ((abs-path (dragonruby--resolve-asset-path raw-path)))
              (if (file-exists-p abs-path)
                  (dragonruby--make-sprite-overlay start end raw-path abs-path 'valid)
                (dragonruby--make-sprite-overlay start end raw-path abs-path 'missing))))
           ((member ext dragonruby-unsupported-sprites)
            (dragonruby--make-sprite-overlay start end raw-path nil 'unsupported))))))))

(defun dragonruby--after-sprite-change (_beg _end _len)
  (dragonruby--scan-sprites))

(define-minor-mode dragonruby-sprite-mode
  "Sprite previews."
  :lighter " DR-Sprite"
  (if dragonruby-sprite-mode
      (progn
        (add-hook 'after-change-functions #'dragonruby--after-sprite-change nil t)
        (add-hook 'completion-at-point-functions #'dragonruby-sprite-completion-at-point nil t)
        (dragonruby--scan-sprites))
    (remove-hook 'after-change-functions #'dragonruby--after-sprite-change t)
    (remove-hook 'completion-at-point-functions #'dragonruby-sprite-completion-at-point t)
    (mapc #'delete-overlay dragonruby--sprite-overlays)))

(provide 'dragonruby-sprites)
