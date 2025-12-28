;;; dragonruby-sprites.el --- Sprite previews and completion -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'image-file)
(require 'dragonruby-core)

(declare-function w32-shell-execute "w32fns.c")

(defvar-local dragonruby--sprite-overlays nil
  "List of sprite overlays in the current buffer.")
(defvar dragonruby-supported-sprites '("png" "jpg" "jpeg" "gif" "bmp"))
(defvar dragonruby-unsupported-sprites '("svg" "psd" "xcf" "ase" "aseprite"))


;; --- PATH RESOLUTION ---
;; Note: dragonruby--find-project-root is now in dragonruby-core.el

(defun dragonruby--resolve-asset-path (path)
  (let ((root (dragonruby--find-project-root)))
    (expand-file-name path root)))

(defun dragonruby-jump-to-sprite-source (path)
  "Open the source file for PATH if it exists, otherwise open PATH directly.
Checks extensions in `dragonruby-sprite-source-extensions`."
  (interactive "fOpen sprite: ")
  (let ((source (and dragonruby-experimental-smart-jump 
                     (dragonruby--find-source-file path))))
    (if source
        (progn
          (message "Found source file: %s" (file-name-nondirectory source))
          (cond
           ((eq system-type 'windows-nt)
            (w32-shell-execute "open" source))
           ((eq system-type 'darwin)
            (start-process "open" nil "open" source))
           (t
            (start-process "xdg-open" nil "xdg-open" source))))
      ;; Fallback: Open the PNG/JPG inside Emacs
      (find-file path))))

;; --- AUTOCOMPLETE (CAPF) ---
(defun dragonruby--get-all-sprites-in-project ()
  (let* ((root (dragonruby--find-project-root))
         (files (directory-files-recursively root "\\.\\(png\\|jpg\\|jpeg\\|gif\\|bmp\\|PNG\\|JPG\\)$")))
    (mapcar (lambda (f) (file-relative-name f root)) files)))

(defun dragonruby-sprite-completion-at-point ()
  "Completion at point function for sprite paths."
  (when (nth 3 (syntax-ppss))  ; inside a string?
    (let* ((string-start (1+ (nth 8 (syntax-ppss))))  ; after opening quote
           (point-now (point)))
      (when (> point-now string-start)
        (list string-start
              point-now
              (completion-table-dynamic
               (lambda (_) (dragonruby--get-all-sprites-in-project)))
              :exclusive 'no)))))

;; --- VISUALS ---
(defun dragonruby--create-tooltip-string (path)
  (if (file-exists-p path)
      (let* ((image (create-image path nil nil :max-width 300 :max-height 300))
             (attrs (file-attributes path))
             (size (file-size-human-readable (file-attribute-size attrs)))
             (info-text (format " \n📦 %s\n📏 %s" (upcase (file-name-extension path)) size)))
        (concat (propertize " " 'display image) info-text))
    "❌ Text file not found"))

(defun dragonruby--create-inline-thumb (path)
  (when (file-exists-p path)
    (let ((image (create-image path nil nil :height 20 :ascent 'center)))
      (concat (propertize " " 'display '(space :width 1))
              (propertize " " 'display image)))))

(defun dragonruby--make-sprite-overlay (start end _text path status)
  (let ((ov (make-overlay start end))
        (color (pcase status ('valid "cyan") ('missing "red") ('unsupported "orange")))
        (style (if (eq status 'valid) nil 'wave))
        (tooltip (if (eq status 'valid) (dragonruby--create-tooltip-string path) "Missing/Invalid")))
    
    (overlay-put ov 'face `(:underline (:color ,color :style ,style)))
    (overlay-put ov 'help-echo tooltip)
    (when (eq status 'valid)
      (let ((thumb (dragonruby--create-inline-thumb path)))
        (when thumb (overlay-put ov 'after-string thumb)))
      (overlay-put ov 'keymap 
                   (let ((map (make-sparse-keymap)))
                     ;; CHANGED: Use new jump function instead of find-file
                     (define-key map [mouse-1] (lambda () (interactive) (dragonruby-jump-to-sprite-source path)))
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

(defun dragonruby--after-sprite-change (beg _end _len)
  "Clear overlays on edited line only, then debounce sprite re-scanning."
  ;; Only clear overlays on the current line (not all)
  (let ((line-start (save-excursion (goto-char beg) (line-beginning-position)))
         (line-end (save-excursion (goto-char beg) (line-end-position))))
    (dolist (ov dragonruby--sprite-overlays)
      (when (and (overlay-start ov)
                 (<= line-start (overlay-start ov))
                 (<= (overlay-end ov) line-end))
        (delete-overlay ov)
        (setq dragonruby--sprite-overlays (delq ov dragonruby--sprite-overlays)))))
  ;; Debounce the re-scan
  (dragonruby--debounce #'dragonruby--scan-sprites 0.3))

(defun dragonruby--refresh-sprites ()
  "Refresh sprite overlays when buffer becomes visible."
  (when (and dragonruby-sprite-mode
             (eq (current-buffer) (window-buffer)))
    (dragonruby--scan-sprites)))

(defun dragonruby--setup-capf ()
  (add-hook 'completion-at-point-functions #'dragonruby-sprite-completion-at-point nil t))

(define-minor-mode dragonruby-sprite-mode
  "Sprite previews."
  :lighter ""
  (if dragonruby-sprite-mode
      (progn
        (add-hook 'after-change-functions #'dragonruby--after-sprite-change nil t)
        (add-hook 'window-configuration-change-hook #'dragonruby--refresh-sprites nil t)
        (dragonruby--setup-capf)
        (dragonruby--scan-sprites))
    (remove-hook 'after-change-functions #'dragonruby--after-sprite-change t)
    (remove-hook 'window-configuration-change-hook #'dragonruby--refresh-sprites t)
    (remove-hook 'completion-at-point-functions #'dragonruby-sprite-completion-at-point t)
    (mapc #'delete-overlay dragonruby--sprite-overlays)
    (setq dragonruby--sprite-overlays nil)))

(provide 'dragonruby-sprites)
