;;; dragonruby-sprite-overlay.el --- Visual representation for sprites -*- lexical-binding: t; -*-

(require 'dragonruby-core)
(require 'dragonruby-sprite-model)
(require 'dragonruby-sprite-fs)
(require 'dragonruby-sprite-actions)
(require 'dragonruby-sprite-popup)

(defvar-local dragonruby--sprite-overlays nil
  "List of sprite overlays in the current buffer.")

(defun dragonruby--clear-sprite-overlays ()
  "Remove all sprite overlays from current buffer using property-based cleanup."
  (remove-overlays (point-min) (point-max) 'dragonruby-sprite t)
  (setq dragonruby--sprite-overlays nil))

;; --- VISUALS ---

(defun dragonruby--create-tooltip-string (path)
  "Create lightweight tooltip with dimensions and size."
  (if (and path (file-exists-p path))
      (let* ((attrs (file-attributes path))
             (size (file-size-human-readable (or (file-attribute-size attrs) 0)))
             (ext (upcase (or (file-name-extension path) "IMG")))
             ;; Calculate dimensions (fast check)
             (img (when (and (file-readable-p path) (> (file-attribute-size attrs) 0))
                    (let ((inhibit-message t))
                      (ignore-errors 
                        (create-image path (dragonruby--get-image-type path) nil :nostrap t)))))
             (dims (if img (image-size img t) '(0 . 0)))
             (w (if (consp dims) (truncate (car dims)) 0))
             (h (if (consp dims) (truncate (cdr dims)) 0)))
        (propertize (format "📐 %dx%d | 📏 %s | 🖼️ %s" w h size ext)
                    'face '(:weight bold)))
    (if path "❌ File not found" "⚠️ Invalid")))

(defun dragonruby--create-inline-thumb (path)
  "Create a subtle inline thumbnail for PATH as a validity indicator.
Scales dynamically with the current font size (Accessibility)."
   (when (and path (display-graphic-p) (file-exists-p path))
     (let* ((attrs (file-attributes path))
            (image (when (and (file-readable-p path) (> (file-attribute-size attrs) 0))
                     (let ((inhibit-message t)) 
                       (ignore-errors 
                         (create-image path (dragonruby--get-image-type path) nil 
                                     :max-height 36 :ascent 'center :background "none"
                                     :transform-smoothing t))))))
      (when image
        (concat (propertize " " 'display '(space :width 1))
                (propertize " " 'display image)
                (propertize " " 'display '(space :width 1)))))))

;; --- PREVIEW POPUP ---

(defun dragonruby-preview-sprite-at-point ()
  "Show a large preview of the sprite at point in a popup buffer."
  (interactive)
  (let* ((ovs (overlays-at (point)))
         (sprite-path nil))
    ;; Find the sprite overlay
    (dolist (ov ovs)
      (when (overlay-get ov 'dragonruby-sprite-path)
        (setq sprite-path (overlay-get ov 'dragonruby-sprite-path))))
    
    (if (and sprite-path (file-exists-p sprite-path))
        (let ((buf (get-buffer-create "*Sprite Preview*")))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert "\n")
              ;; Insert large image (balanced size for quick preview)
              (let* ((attrs (file-attributes sprite-path))
                     (img (when (and (file-readable-p sprite-path) (> (file-attribute-size attrs) 0))
                            (let ((inhibit-message t))
                              (ignore-errors
                                (create-image sprite-path nil nil :max-width 400 :max-height 400))))))
                (if img
                    (insert-image img)
                  (insert (propertize "⚠️ Unable to load image preview." 'face '(:foreground "orange")))))
              (insert "\n\n")
              (insert (format "📁 %s\n" sprite-path))
              (insert "\n[q] Close  [RET] Open file"))
            (local-set-key (kbd "q") 'quit-window)
            (local-set-key (kbd "RET") `(lambda () (interactive) (quit-window) (find-file ,sprite-path)))
            (goto-char (point-min)))
          (display-buffer buf '(display-buffer-pop-up-window)))
      (message "No sprite at point"))))

;; --- OVERLAY CREATION ---

(defun dragonruby--make-sprite-overlay (start end raw-path path status)
  "Create overlay from START to END for sprite PATH with STATUS."
  (let ((ov (make-overlay start end)))
    
    ;; SIMPLIFIED VISUAL LANGUAGE:
    ;; - Valid: NO underline, only mini-icon (clean look)
    ;; - Missing/Unsupported: Wavy underline (visual error indicator)
    (unless (eq status 'valid)
      (let ((color (pcase status ('missing "red") ('unsupported "orange"))))
        (overlay-put ov 'face `(:underline (:color ,color :style wave)))))
    
    (overlay-put ov 'dragonruby-sprite t)
    (overlay-put ov 'dragonruby-sprite-path path)
    
    ;; TOOLTIP LOGIC (For ALL statuses)
    (let ((msg (pcase status
                 ('valid (dragonruby--create-tooltip-string path))
                 ('missing (format "❌ File not found: %s" path))
                 ('unsupported (format "⚠️ Unsupported format: .%s\n(Valid for DragonRuby, but no preview in Emacs)" 
                                       (file-name-extension (or path raw-path))))
                 (_ "Unknown status"))))
      (overlay-put ov 'help-echo msg))

    (when (eq status 'valid)
      ;; 1. Fast, Cached Thumbnail (small)
      (let ((thumb (dragonruby--create-inline-thumb path)))
        (when thumb (overlay-put ov 'after-string thumb)))
        
      (overlay-put ov 'keymap 
                   (let ((map (make-sparse-keymap)))
                     (define-key map (kbd "C-c C-o") 
                       (lambda () (interactive) (dragonruby-jump-to-sprite-source path)))
                     (define-key map (kbd "RET") 
                       (lambda () (interactive) (dragonruby-jump-to-sprite-source path)))
                     map))
      (overlay-put ov 'mouse-face '(:background "#2ECC71" :foreground "black")))
    (overlay-put ov 'priority -50)
    (push ov dragonruby--sprite-overlays)))

;; --- SCANNING ---

(defun dragonruby--scan-sprites (&optional beg end)
  "Scan buffer for sprite strings and create overlays.
If BEG and END are provided, only scan that region for performance."
  (let ((search-beg (or beg (point-min)))
        (search-end (or end (point-max))))
    ;; Only clear overlays in the targeted region
    (remove-overlays search-beg search-end 'dragonruby-sprite t)
    (let ((root (dragonruby--find-project-root)))
      (when root
        (save-excursion
          (save-restriction
            (narrow-to-region search-beg search-end)
            (goto-char (point-min))
            (while (re-search-forward "\"\\([^\"\n]+\\)\"" nil t)
            (let* ((start (match-beginning 1))
                   (end (match-end 1))
                   (raw-path (match-string 1))
                   (ext (file-name-extension raw-path)))
              ;; (message "DEBUG: [Scan] Quoted String: %s (Ext: %s)" raw-path ext)
              (when ext
                (setq ext (downcase ext))
                (cond
                 ((member ext dragonruby-supported-sprites)
                  (let ((abs-path (dragonruby--resolve-path raw-path 'sprite)))
                    (if (and abs-path (file-exists-p abs-path))
                        (dragonruby--make-sprite-overlay start end raw-path abs-path 'valid)
                      (dragonruby--make-sprite-overlay start end raw-path abs-path 'missing))))
                 ((member ext dragonruby-unsupported-sprites)
                  (dragonruby--make-sprite-overlay start end raw-path nil 'unsupported))))))))))))

(defun dragonruby--after-sprite-change (beg end _len)
  "Clear overlays on edited area and debounce incremental re-scanning."
  (let ((line-beg (save-excursion (goto-char beg) (line-beginning-position)))
        (line-end (save-excursion (goto-char end) (line-end-position))))
    ;; Targeted re-scan: just the lines affected
    (dragonruby--debounce 'sprites-incremental 
                          (lambda () (dragonruby--scan-sprites line-beg line-end)) 
                          0.1)
    ;; Global catch-up scan: much slower to avoid stutters while typing
    (dragonruby--debounce 'sprites-global #'dragonruby--scan-sprites 1.0)))

(defun dragonruby--refresh-sprites ()
  "Refresh sprite overlays when buffer becomes visible."
  (when (and dragonruby-mode
             (eq (current-buffer) (window-buffer)))
    (dragonruby--scan-sprites)))

(provide 'dragonruby-sprite-overlay)
