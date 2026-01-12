;;; dragonruby-sprite-overlay.el --- Visual representation for sprites -*- lexical-binding: t; -*-

(require 'dragonruby-core)
(require 'dragonruby-sprite-model)
(require 'dragonruby-sprite-fs)
(require 'dragonruby-sprite-actions)
(require 'dragonruby-sprite-popup)

(defcustom dragonruby-sprite-thumbnail-size 20
  "Fixed height in pixels for inline sprite thumbnails.
Default: 20px."
  :type 'integer
  :group 'dragonruby)

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
        (propertize (format "📐 %dx%d | 📏 %s | 🖼️ %s\n💡 Click to open | [RET] Quick View" w h size ext)
                    'face '(:weight bold)))
    (if path "❌ File not found" "⚠️ Invalid")))

(defun dragonruby--create-inline-thumb (path)
  "Create a fixed-size inline thumbnail for PATH."
   (when (and path (display-graphic-p) (file-exists-p path))
     (let* ((attrs (file-attributes path))
            (image (when (and (file-readable-p path) (> (file-attribute-size attrs) 0))
                     (let ((inhibit-message t)) 
                       (ignore-errors 
                         (create-image path (dragonruby--get-image-type path) nil 
                                     :max-height dragonruby-sprite-thumbnail-size
                                     :ascent 'center 
                                     :background "none"
                                     :transform-smoothing t))))))
      (when image
        (concat (propertize " " 'display '(space :width 1))
                (propertize " " 'display image)
                (propertize " " 'display '(space :width 1)))))))

;; --- OVERLAY CREATION ---

(defun dragonruby--make-sprite-overlay (start end raw-path path status)
  "Create overlay from START to END for sprite PATH with STATUS."
  (let ((ov (make-overlay start end)))
    (unless (eq status 'valid)
      (let ((color (pcase status ('missing "red") ('unsupported "orange"))))
        (overlay-put ov 'face `(:underline (:color ,color :style wave)))))
    
    (overlay-put ov 'dragonruby-sprite t)
    (overlay-put ov 'dragonruby-sprite-path path)
    
    ;; Tooltip simple de texto (responsable y ligero)
    (overlay-put ov 'help-echo (pcase status
                                 ('valid (dragonruby--create-tooltip-string path))
                                 ('missing (format "❌ File not found: %s" path))
                                 ('unsupported (format "⚠️ Unsupported format: .%s" 
                                                       (file-name-extension (or path raw-path))))
                                 (_ "Unknown status")))

    (when (eq status 'valid)
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
Called ONLY by the central scheduler."
  (let ((search-beg (or beg (point-min)))
        (search-end (or end (point-max))))
    (remove-overlays search-beg search-end 'dragonruby-sprite t)
    (let ((root (dragonruby--find-project-root)))
      (when root
        (save-excursion
          (goto-char search-beg)
          (while (re-search-forward "\"\\([^\"\n]+\\)\"" search-end t)
            (let* ((start (match-beginning 1))
                   (end (match-end 1))
                   (raw-path (match-string 1))
                   (ext (file-name-extension raw-path)))
              (when ext
                (setq ext (downcase ext))
                (cond
                 ((member ext dragonruby-supported-sprites)
                  (let ((abs-path (dragonruby--resolve-path raw-path 'sprite)))
                    (if (and abs-path (file-exists-p abs-path))
                        (dragonruby--make-sprite-overlay start end raw-path abs-path 'valid)
                      (dragonruby--make-sprite-overlay start end raw-path abs-path 'missing))))
                 ((member ext dragonruby-unsupported-sprites)
                  (dragonruby--make-sprite-overlay start end raw-path nil 'unsupported)))))))))))

(provide 'dragonruby-sprite-overlay)
;;; dragonruby-sprite-overlay.el ends here
