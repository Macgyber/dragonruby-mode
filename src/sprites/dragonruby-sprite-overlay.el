;;; dragonruby-sprite-overlay.el --- Visual representation for sprites -*- lexical-binding: t; -*-

(require 'dragonruby-core)
(require 'dragonruby-sprite-model)
(require 'dragonruby-sprite-fs)
(require 'dragonruby-sprite-actions)

(defvar-local dragonruby--sprite-overlays nil
  "List of sprite overlays in the current buffer.")

(defun dragonruby--clear-sprite-overlays ()
  "Remove all sprite overlays from current buffer using property-based cleanup."
  (remove-overlays (point-min) (point-max) 'dragonruby-sprite t)
  (setq dragonruby--sprite-overlays nil))

;; --- VISUALS ---

(defun dragonruby--create-tooltip-string (path)
  "Create text-only tooltip with metadata for PATH (LSP-safe, non-intrusive)."
  (if (and path (file-exists-p path))
      (let* ((attrs (file-attributes path))
             (size (file-size-human-readable (file-attribute-size attrs)))
             (ext (upcase (file-name-extension path))))
        (format "🖼️ %s | 📏 %s\n(C-c C-o to open)" ext size))
    (if path "❌ File not found" "⚠️ Unsupported or Invalid Format")))

(defun dragonruby--create-inline-thumb (path)
  "Create a subtle inline thumbnail for PATH as a validity indicator.
Scales dynamically with the current font size (Accessibility)."
  (when (and path (display-graphic-p) (file-exists-p path))
    (let* ((fh (window-font-height)) ; Real-time pixel height including zoom
           ;; Thumb should be ~85% of line height to be visible but not break layout
           (size (max 14 (floor (* fh 0.85))))
           (image (create-image path nil nil :height size :ascent 'center)))
      (concat (propertize " " 'display '(space :width 1))
              (propertize " " 'display image)))))

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
              (let ((img (create-image sprite-path nil nil :max-width 400 :max-height 400)))
                (insert-image img))
              (insert "\n\n")
              (insert (format "📁 %s\n" sprite-path))
              (insert "\n[q] Close  [RET] Open file"))
            (local-set-key (kbd "q") 'quit-window)
            (local-set-key (kbd "RET") `(lambda () (interactive) (quit-window) (find-file ,sprite-path)))
            (goto-char (point-min)))
          (display-buffer buf '(display-buffer-pop-up-window)))
      (message "No sprite at point"))))

;; --- OVERLAY CREATION ---

(defun dragonruby--make-sprite-overlay (start end _text path status)
  "Create overlay from START to END for sprite PATH with STATUS."
  (let ((ov (make-overlay start end))
        (color (pcase status ('valid "cyan") ('missing "red") ('unsupported "orange")))
        (style (if (eq status 'valid) nil 'wave))
        (tooltip (dragonruby--create-tooltip-string path)))
    
    (overlay-put ov 'face `(:underline (:color ,color :style ,style)))
    (overlay-put ov 'help-echo tooltip)
    (overlay-put ov 'dragonruby-sprite t)
    (overlay-put ov 'dragonruby-sprite-path path)
    (when (eq status 'valid)
      ;; Restore visual indicator (thumbnail)
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

(defun dragonruby--scan-sprites ()
  "Scan buffer for sprite strings and create overlays."
  (dragonruby--clear-sprite-overlays)
  (let ((root (dragonruby--find-project-root)))
    (when root
      (save-excursion
        (save-restriction
          (widen)
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
                    (if (and abs-path (file-exists-p abs-path))
                        (dragonruby--make-sprite-overlay start end raw-path abs-path 'valid)
                      (dragonruby--make-sprite-overlay start end raw-path abs-path 'missing))))
                 ((member ext dragonruby-unsupported-sprites)
                  (dragonruby--make-sprite-overlay start end raw-path nil 'unsupported)))))))))))

(defun dragonruby--after-sprite-change (beg end _len)
  "Clear overlays on edited area and debounce sprite re-scanning."
  (save-match-data
    (let ((line-beg (save-excursion (goto-char beg) (line-beginning-position)))
          (line-end (save-excursion (goto-char end) (line-end-position))))
      (remove-overlays line-beg line-end 'dragonruby-sprite t)))
  (dragonruby--debounce 'sprites #'dragonruby--scan-sprites 0.15))

(defun dragonruby--refresh-sprites ()
  "Refresh sprite overlays when buffer becomes visible."
  (when (and (bound-and-true-p dragonruby-sprite-mode)
             (eq (current-buffer) (window-buffer)))
    (dragonruby--scan-sprites)))

(provide 'dragonruby-sprite-overlay)
