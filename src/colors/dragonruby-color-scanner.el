;;; dragonruby-color-scanner.el --- Regex patterns to find colors in code -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'dragonruby-color-visuals)
(require 'dragonruby-color-utils)

(defun dragonruby--scan-rgba-hash-at (pos)
  "Scan for r, g, b, a properties in a hash starting around POS.
Returns the end position of the scanned block to allow jumping."
  (save-excursion
    (goto-char pos)
    (when (dragonruby--in-code-p)
      (let* ((brace-limit (save-excursion 
                           (ignore-errors 
                             (up-list 1) 
                             (point))))
             (limit (if brace-limit 
                        (min brace-limit (+ pos 500))
                      (+ pos 300)))
            r g b a (matches '()))
        
        ;; Look ahead for r, g, b, a keys within the limit
        (while (re-search-forward
                "\\b\\([rgba]\\)\\s-*:\\s-*\\([0-9]+\\)"
                limit t)
          (when (dragonruby--in-code-p)
            (push (list (match-beginning 0)
                        (match-end 0)
                        (match-string 1)
                        (string-to-number (match-string 2)))
                  matches)))
        
        ;; Collect values
        (setq r nil g nil b nil a nil)
        (dolist (m matches)
          (pcase (nth 2 m)
            ("r" (setq r (nth 3 m)))
            ("g" (setq g (nth 3 m)))
            ("b" (setq b (nth 3 m)))
            ("a" (setq a (nth 3 m)))))
        
        ;; If we have at least RGB, paint it
        (if (and r g b)
            (let ((start (apply #'min (mapcar #'car matches)))
                  (end   (apply #'max (mapcar #'cadr matches))))
              ;; FIX: Avoid "coloring spaces" in multiline hashes
              (if (= (line-number-at-pos start) (line-number-at-pos end))
                  ;; Same line: single overlay is fine and cleaner
                  (dragonruby--make-color-overlay start end r g b a)
                ;; Multi-line: individual overlays to avoid highlighting spaces/newlines
                (dolist (m matches)
                  (dragonruby--make-color-overlay (nth 0 m) (nth 1 m) r g b a)))
              end)
          nil)))))

(defun dragonruby--scan-colors ()
  "Main entry point for color scanning. Scans arrays, hex (0x and #), and hashes."
  (dragonruby--clear-color-overlays)
  (save-excursion

    ;; 1. Arrays RGB / RGBA: [255, 0, 0] or [255, 0, 0, 255]
    (goto-char (point-min))
    (while (re-search-forward
            "\\[\\s-*\\([0-9]+\\)\\s-*,\\s-*\\([0-9]+\\)\\s-*,\\s-*\\([0-9]+\\)\\(?:\\s-*,\\s-*\\([0-9]+\\)\\)?\\s-*\\]"
            nil t)
      (when (dragonruby--in-code-p)
        (dragonruby--make-color-overlay
         (match-beginning 0) (match-end 0)
         (string-to-number (match-string 1))
         (string-to-number (match-string 2))
         (string-to-number (match-string 3))
         (when (match-string 4)
           (string-to-number (match-string 4))))))

    ;; 2. Hexadecimal (0xRRGGBB and #RRGGBB)
    ;; Note: DragonRuby usually doesn't use these for primitives, but they are common in Ruby.
    (goto-char (point-min))
    (while (re-search-forward "\\(?:0x\\|#\\)\\([0-9A-Fa-f]\\{6\\}\\)" nil t)
      (when (dragonruby--in-code-p)
        (let* ((hex (match-string 1))
               (r (string-to-number (substring hex 0 2) 16))
               (g (string-to-number (substring hex 2 4) 16))
               (b (string-to-number (substring hex 4 6) 16)))
          (dragonruby--make-color-overlay
           (match-beginning 0) (match-end 0) r g b))))

    ;; 3. Hash RGB / RGBA: { r: 255, g: 0, b: 0 }
    (goto-char (point-min))
    (while (re-search-forward "\\b[rgb]\\s-*:" nil t)
      (let ((jump-pos (dragonruby--scan-rgba-hash-at (match-beginning 0))))
        (if jump-pos
            (goto-char jump-pos)
          (forward-char 1))))))

(provide 'dragonruby-color-scanner)
