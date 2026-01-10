;;; dragonruby-concept-visuals.el --- Visual overlays for concepts -*- lexical-binding: t; -*-

(require 'dragonruby-core)
(require 'dragonruby-docs)

(defvar dragonruby-concept-keywords
  '("args" "outputs" "inputs" "state" "grid" "geometry" "tick" "sprites" "solids" "borders" "labels")
  "List of DragonRuby concepts to highlight.")

(defvar-local dragonruby--concept-overlays nil
  "List of concept overlays in the current buffer.")

(defun dragonruby--clear-concept-overlays ()
  "Remove all concept overlays from current buffer using property-based cleanup."
  (remove-overlays (point-min) (point-max) 'dragonruby-concept t)
  (setq dragonruby--concept-overlays nil))

(require 'dragonruby-knowledge)

(defun dragonruby--activate-concept (concept-name)
  "Action triggered when interacting with a concept."
  (let ((info (dragonruby-knowledge-get concept-name)))
    (if info
        (with-current-buffer (get-buffer-create "*DragonRuby Knowledge*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "🧠 Knowledge Card: %s\n" (upcase concept-name)))
            (insert (make-string 40 ?─))
            (insert "\n\n")
            (insert info)
            (insert "\n\n")
            (insert (make-string 40 ?─))
            (insert "\nPress 'q' to close."))
          (special-mode)
          (display-buffer (current-buffer)))
      (message "No knowledge card for '%s'" concept-name))))

(defun dragonruby--make-concept-overlay (start end concept-name)
  (let ((ov (make-overlay start end))
        (info (dragonruby-knowledge-get concept-name)))
    (when info
      ;; Visual Style: Subtle Blue Dotted Underline (Lightweight)
      (overlay-put ov 'face '(:underline (:color "#61AFEF" :style wave)))
      
      ;; Tooltip: The Knowledge Card
      (overlay-put ov 'help-echo (format "%s\n\n(C-c C-o to learn more)" info))
      
      ;; Semantic Property
      (overlay-put ov 'dragonruby-concept t)
      
      ;; Priority
      (overlay-put ov 'priority 50)
      
      ;; Interaction Map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c C-o") 
          (lambda () (interactive) (dragonruby--activate-concept concept-name)))
        (overlay-put ov 'keymap map))
      
      (push ov dragonruby--concept-overlays))))

(defcustom dragonruby-concepts-debug nil
  "Enable atomic debug messages for concept scanner."
  :type 'boolean
  :group 'dragonruby-concepts)

(defun dragonruby--scan-concepts ()
  (dragonruby--clear-concept-overlays)
  (when dragonruby-concepts-debug
    (message "🧠 [Concepts] Atomic Scan Started in %s" (buffer-name)))
    
  (save-excursion
    (goto-char (point-min))
    (let ((regexp (concat "\\_<" (regexp-opt dragonruby-concept-keywords) "\\_>"))
          (count 0))
      (while (re-search-forward regexp nil t)
        (let ((concept (match-string 0))
              (start (match-beginning 0))
              (end (match-end 0)))
          ;; Check context: Ensure we are not inside a string or comment
          (let ((ppss (syntax-ppss)))
            (unless (or (nth 3 ppss) (nth 4 ppss))
              (setq count (1+ count))
              (when dragonruby-concepts-debug
                (message "🧠 [Concepts] Found atomic '%s' at %d" concept start))
              (dragonruby--make-concept-overlay start end concept)))))
      
      (when dragonruby-concepts-debug
        (message "🧠 [Concepts] Scan Complete. Total atoms: %d" count)))))

(defun dragonruby--after-concept-change (beg end _len)
  "Invalidate concepts in edited range and trigger rescanning."
  (save-match-data
    (remove-overlays beg end 'dragonruby-concept t))
  (dragonruby--debounce 'concepts #'dragonruby--scan-concepts 0.5))

(defun dragonruby--refresh-concepts ()
  (when (and dragonruby-mode
             (eq (current-buffer) (window-buffer)))
    (dragonruby--scan-concepts)))

(provide 'dragonruby-concept-visuals)
