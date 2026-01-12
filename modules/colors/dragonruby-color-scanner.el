;;; dragonruby-color-scanner.el --- Regex patterns to find colors in code -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'dragonruby-color-visuals)
(require 'dragonruby-color-utils)

(defun dragonruby--scan-colors (&optional beg end)
  "Scan the buffer for color definitions and apply overlays.
BEG and END define the range; defaults to whole buffer."
  (when (and (bound-and-true-p dragonruby-mode) 
             (bound-and-true-p dragonruby-enable-colors))
    (let* ((start-pos (or beg (point-min)))
           (end-pos (or end (point-max)))
           ;; Atomic key-value regex (r: 100, :r => 100, etc)
           (rgba-regex "\\(?:\\b\\|[ \t,]\\)[:\"']?\\([rgba]\\)[:\"']?\\s-*\\(?:=>\\|:\\)\\s-*\\([0-9]+\\)")
           (all-matches '()))
      
      (remove-overlays start-pos end-pos 'dragonruby-color t)
      (save-excursion
        ;; 1. Collect all local components
        (goto-char start-pos)
        (while (re-search-forward rgba-regex end-pos t)
          (let ((m-start (match-beginning 1))
                (m-end (match-end 0))
                (key (match-string 1))
                (val (string-to-number (match-string 2))))
            (save-excursion
              (goto-char m-start)
              (when (dragonruby--in-code-p)
                (push (list m-start m-end key val) all-matches)))))

        ;; 2. Surgical Clustering (ID-based isolation)
        (let ((clusters '()))
          (setq all-matches (sort all-matches (lambda (a b) (< (car a) (car b)))))
          (let ((current-cluster '())
                (keys-in-cluster '()))
            (dolist (m all-matches)
              (let ((key (nth 2 m))
                    (pos (car m)))
                ;; Restart cluster if:
                ;; - Too far from previous (> 80 chars)
                ;; - Key is already in cluster (indicates new object)
                (if (or (null current-cluster)
                        (and (> (- pos (cadr (car (last current-cluster)))) 80))
                        (member key keys-in-cluster))
                    (progn
                      (when current-cluster (push (reverse current-cluster) clusters))
                      (setq current-cluster (list m))
                      (setq keys-in-cluster (list key)))
                  (push m current-cluster)
                  (push key keys-in-cluster))))
            (when current-cluster (push (reverse current-cluster) clusters)))

          ;; 3. Paint Clusters (COHESIVE LINE RENDERING)
          (dolist (cluster clusters)
            (let (r g b a)
              (dolist (m cluster)
                (pcase (nth 2 m) ("r" (setq r (nth 3 m))) ("g" (setq g (nth 3 m)))
                                 ("b" (setq b (nth 3 m))) ("a" (setq a (nth 3 m)))))
              
              (when (and r g b (dragonruby--valid-rgba-p r g b a))
                (let ((lines (make-hash-table :test 'equal)))
                  ;; Group matches by line number
                  (dolist (m cluster)
                    (let ((line (line-number-at-pos (car m))))
                      (puthash line (cons m (gethash line lines)) lines)))
                  
                  ;; For each line, create a single continuous bar
                  (maphash (lambda (_line m-list)
                             (let* ((sorted (sort m-list (lambda (a b) (< (car a) (car b)))))
                                    (line-start (car (car sorted)))
                                    (line-end (cadr (car (last sorted)))))
                               ;; Aesthetic: expand to cover the final comma of the line
                               (save-excursion
                                 (goto-char line-end)
                                 (when (looking-at "[ \t]*,") (setq line-end (match-end 0))))
                               (dragonruby--make-color-overlay line-start line-end r g b a)))
                           lines))))))

        ;; 4. Array Primitives
        (goto-char start-pos)
        (while (re-search-forward "\\[\\s-*\\([0-9]+\\)\\s-*,\\s-*\\([0-9]+\\)\\s-*,\\s-*\\([0-9]+\\)\\(?:\\s-*,\\s-*\\([0-9]+\\)\\)?\\s-*\\]" end-pos t)
          (when (dragonruby--in-code-p)
            (let ((r (string-to-number (match-string 1)))
                  (g (string-to-number (match-string 2)))
                  (b (string-to-number (match-string 3)))
                  (a (and (match-string 4) (string-to-number (match-string 4)))))
              (when (dragonruby--valid-rgba-p r g b a)
                (dragonruby--make-color-overlay (match-beginning 0) (match-end 0) r g b a)))))

        ;; 5. Solid/Sprite Primitives [x,y,w,h,r,g,b]
        (goto-char start-pos)
        (while (re-search-forward "\\[\\s-*[0-9-.]+\\s-*,\\s-*[0-9-.]+\\s-*,\\s-*[0-9-.]+\\s-*,\\s-*[0-9-.]+\\s-*,\\s-*\\([0-9]+\\)\\s-*,\\s-*\\([0-9]+\\)\\s-*,\\s-*\\([0-9]+\\)\\(?:\\s-*,\\s-*\\([0-9]+\\)\\)?\\s-*\\]" end-pos t)
          (when (dragonruby--in-code-p)
            (let ((r (string-to-number (match-string 1)))
                  (g (string-to-number (match-string 2)))
                  (b (string-to-number (match-string 3)))
                  (a (and (match-string 4) (string-to-number (match-string 4)))))
              (when (dragonruby--valid-rgba-p r g b a)
                (dragonruby--make-color-overlay (match-beginning 0) (match-end 0) r g b a)))))

        ;; 6. Hexadecimal Literals
        (goto-char start-pos)
        (while (re-search-forward "\\(?:0x\\|#\\)\\([0-9A-Fa-f]\\{6\\}\\)" end-pos t)
          (when (dragonruby--in-code-p)
            (let* ((hex (match-string 1))
                   (r (string-to-number (substring hex 0 2) 16))
                   (g (string-to-number (substring hex 2 4) 16))
                   (b (string-to-number (substring hex 4 6) 16)))
              (dragonruby--make-color-overlay (match-beginning 0) (match-end 0) r g b))))))))

(provide 'dragonruby-color-scanner)
