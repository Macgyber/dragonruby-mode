;;; dragonruby-colors.el --- Professional Color Scanning (Refactored) -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)

(defvar dragonruby--color-overlays nil)
(defvar dragonruby-colors-table (make-hash-table :test 'equal))

;; --- JSON LOADING ---

(defun dragonruby--find-root ()
  (let ((start-dir (file-name-directory (or load-file-name buffer-file-name default-directory))))
    (or (locate-dominating-file start-dir "src")
        start-dir)))

(defvar dragonruby-palettes-file 
  (let ((root (dragonruby--find-root)))
    (if root (expand-file-name "src/data/palettes.json" root) nil)))

(defun dragonruby--load-palettes ()
  (clrhash dragonruby-colors-table)
  (if (and dragonruby-palettes-file (file-exists-p dragonruby-palettes-file))
      (condition-case err
          (let ((data (let ((json-object-type 'alist)) 
                        (json-read-file dragonruby-palettes-file))))
            (dolist (category data)
              (let ((colors (cdr category)))
                (dolist (color colors)
                  (let ((name (car color))
                        (val (cdr color)))
                    (puthash (symbol-name name) val dragonruby-colors-table)))))
            (message "🎨 DragonRuby: Loaded %d colors." (hash-table-count dragonruby-colors-table)))
        (error (message "⚠️ DragonRuby JSON Error: %s" err)))
    (message "⚠️ DragonRuby: Palettes file not found!")))

(dragonruby--load-palettes)

;; --- OVERLAYS ---

(defun dragonruby--clear-color-overlays ()
  (mapc #'delete-overlay dragonruby--color-overlays)
  (setq dragonruby--color-overlays nil))

(defun dragonruby--get-contrast-color (r g b)
  (if (< (+ (* r 0.299) (* g 0.587) (* b 0.114)) 128) "white" "black"))

(defun dragonruby--rgb-to-hex (r g b)
  (format "#%02x%02x%02x" (min 255 (max 0 r)) (min 255 (max 0 g)) (min 255 (max 0 b))))

(defun dragonruby--make-color-overlay (start end r g b)
  (let ((ov (make-overlay start end))
        (hex (dragonruby--rgb-to-hex r g b))
        (contrast (dragonruby--get-contrast-color r g b)))
    (overlay-put ov 'face `(:background ,hex :foreground ,contrast :box (:line-width -1 :color "grey50")))
    (overlay-put ov 'dragonruby-color t)
    (push ov dragonruby--color-overlays)))

(defun dragonruby--make-simple-overlay (start end hex)
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'face `(:background ,hex :box (:line-width -1 :color "grey50")))
    (overlay-put ov 'dragonruby-color t)
    (push ov dragonruby--color-overlays)))

;; --- SCANNERS ---

(defun dragonruby--process-hash-match (match-start limit)
  "Helper to process a potential hash starting at MATCH-START."
  ;; Determine context (one-line vs multiline)
  (let* ((brace-start (save-excursion (ignore-errors (backward-up-list 1) (point))))
         (brace-end (save-excursion (ignore-errors (up-list 1) (point))))
         (valid-context (and brace-start brace-end 
                             (> match-start brace-start) 
                             (< match-start brace-end)))
         (matches '())
         (r nil) (g nil) (b nil))

    (save-excursion
      (goto-char match-start)
      ;; Search for r, g, b, and optional comma
      (while (re-search-forward "\\b\\([rgb]\\):?\\s-*\\([0-9]+\\)\\s-*\\(,\\)?" limit t)
        (push (list (match-beginning 0) (match-end 0) 
                    (match-string 1) 
                    (string-to-number (match-string 2))) 
              matches))
      
      ;; Verify we have all three components
      (dolist (m matches)
        (cond
         ((string= (nth 2 m) "r") (setq r (nth 3 m)))
         ((string= (nth 2 m) "g") (setq g (nth 3 m)))
         ((string= (nth 2 m) "b") (setq b (nth 3 m)))))
      
      (when (and r g b)
        (if (and valid-context (= (line-number-at-pos brace-start) (line-number-at-pos brace-end)))
            ;; ONE-LINER: Paint full block
            (dragonruby--make-color-overlay brace-start brace-end r g b)
          ;; MULTILINE: Paint fragments
          (dolist (m matches)
            (dragonruby--make-color-overlay (nth 0 m) (nth 1 m) r g b)))
        ;; Return the limit to advance the main loop
        limit))))

(defun dragonruby--scan-colors ()
  (dragonruby--clear-color-overlays)
  (save-excursion
    
    ;; 1. ARRAYS
    (goto-char (point-min))
    (while (re-search-forward "\\[\\s-*\\([0-9]+\\)\\s-*,\\s-*\\([0-9]+\\)\\s-*,\\s-*\\([0-9]+\\)\\(?:\\s-*,\\s-*\\([0-9]+\\)\\)?\\s-*\\]" nil t)
      (dragonruby--make-color-overlay (match-beginning 0) (match-end 0) 
                                      (string-to-number (match-string 1))
                                      (string-to-number (match-string 2))
                                      (string-to-number (match-string 3))))

    ;; 2. HEX
    (goto-char (point-min))
    (while (re-search-forward "0x\\([0-9A-Fa-f]\\{6\\}\\)" nil t)
      (let* ((hex-str (match-string 1))
             (r (string-to-number (substring hex-str 0 2) 16))
             (g (string-to-number (substring hex-str 2 4) 16))
             (b (string-to-number (substring hex-str 4 6) 16)))
        (dragonruby--make-color-overlay (match-beginning 0) (match-end 0) r g b)))

    ;; 3. SYMBOLS
    (goto-char (point-min))
    (while (re-search-forward ":\\([a-zA-Z0-9_]+\\)\\b" nil t)
      (let ((hex (gethash (match-string 1) dragonruby-colors-table)))
        (when hex
          (dragonruby--make-simple-overlay (match-beginning 0) (match-end 0) hex))))

    ;; 4. HASHES (Refactored)
    (goto-char (point-min))
    (while (re-search-forward "\\b\\([rgb]\\):" nil t)
      (let* ((start-pos (match-beginning 0))
             ;; Find limit (brace or arbitrary)
             (brace-end (save-excursion (ignore-errors (up-list 1) (point))))
             (limit (if brace-end (min brace-end (+ (point) 300)) (+ (point) 300))))
        
        ;; Avoid double-processing if overlay exists
        (unless (cl-some (lambda (ov) (and (<= (overlay-start ov) start-pos)
                                           (>= (overlay-end ov) start-pos)))
                         dragonruby--color-overlays)
          
          (let ((new-pos (dragonruby--process-hash-match start-pos limit)))
            (when new-pos
              (goto-char new-pos))))))))

(defun dragonruby--after-color-change (_beg _end _len)
  (dragonruby--scan-colors))

(define-minor-mode dragonruby-color-blocks-mode
  "Smart color highlighting."
  :lighter " DR-Color"
  (if dragonruby-color-blocks-mode
      (progn
        (add-hook 'after-change-functions #'dragonruby--after-color-change nil t)
        (dragonruby--scan-colors))
    (remove-hook 'after-change-functions #'dragonruby--after-color-change t)
    (dragonruby--clear-color-overlays)))

(provide 'dragonruby-colors)
