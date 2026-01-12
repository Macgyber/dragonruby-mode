;;; dragonruby-color-picker.el --- Functional Color Picker (EXPERIMENTAL) -*- lexical-binding: t; -*-
;;
;; ⚠️ STATUS: EXPERIMENTAL
;; This module is a work-in-progress. It provides interactive color selection
;; but the API and interaction model are subject to change.

(require 'dragonruby-color-utils)

(defun dragonruby--hex-to-rgb (hex)
  "Convert #RRGGBB to list (r g b)."
  (let ((r (string-to-number (substring hex 1 3) 16))
        (g (string-to-number (substring hex 3 5) 16))
        (b (string-to-number (substring hex 5 7) 16)))
    (list r g b)))

(defun dragonruby-color-picker-edit (start end format)
  "Open color picker and replace text at START-END based on FORMAT."
  (interactive)
  (let* ((current-text (buffer-substring-no-properties start end))
         ;; Using native read-color for now
         (new-color-name (read-color (format "Choose color (replacing %s): " current-text)))
         ;; Convert whatever emacs gives us (name or hex) to canonical Hex #RRGGBB
         (new-hex (if (string-prefix-p "#" new-color-name)
                      new-color-name
                    (apply #'format "#%02x%02x%02x" (color-values new-color-name))))
         (rgb (dragonruby--hex-to-rgb new-hex))
         (r (nth 0 rgb))
         (g (nth 1 rgb))
         (b (nth 2 rgb))
         
         ;; Construct replacement string based on format
         (replacement 
          (cond
           ((eq format 'array)
            (format "[%d, %d, %d]" r g b))
           
           ((eq format 'hex)
            (format "0x%02X%02X%02X" r g b)) ;; Keep 0x prefix standard
           
           ((eq format 'hash)
            (format "{r: %d, g: %d, b: %d}" r g b))
           
           (t current-text)))) ;; Fallback
    
    (when (not (string= new-color-name ""))
      (delete-region start end)
      (insert replacement)
      (message "🎨 Updated color to %s" replacement))))

(provide 'dragonruby-color-picker)
