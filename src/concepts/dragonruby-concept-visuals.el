;;; dragonruby-concept-visuals.el --- Visual overlays for concepts -*- lexical-binding: t; -*-

(require 'dragonruby-core)
(require 'dragonruby-events)
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

(defun dragonruby--activate-concept (concept-name)
  "Action triggered when interacting with a concept."
  (message "🔍 Concept: %s" concept-name)
  ;; Trigger the documentation system
  (let ((doc-file (dragonruby-docs--find-concept-for-symbol concept-name)))
    (if doc-file
        (dragonruby-docs-open doc-file "definition")
      (message "No documentation entry found for '%s'" concept-name))))

(defun dragonruby--make-concept-overlay (start end concept-name)
  (let ((ov (make-overlay start end)))
    ;; Visual Style: Subtle Blue Underline
    (overlay-put ov 'face '(:underline (:color "#61AFEF" :style line)))
    
    ;; Tooltip
    (overlay-put ov 'help-echo (format "Concept: %s (C-c C-o to view docs)" concept-name))
    
    ;; Semantic Property
    (overlay-put ov 'dragonruby-concept t)
    
    ;; Priority (The Shield): Ensure it sits above basic font-lock but doesn't block everything
    (overlay-put ov 'priority 50)
    
    ;; Interaction Map - Keyboard only (no accidental clicks)
    (let ((map (make-sparse-keymap)))
      ;; Keyboard Interaction
      (define-key map (kbd "C-c C-o") 
        (lambda () (interactive) (dragonruby--activate-concept concept-name)))
      
      (overlay-put ov 'keymap map))
    
    (push ov dragonruby--concept-overlays)))

(defun dragonruby--scan-concepts ()
  (dragonruby--clear-concept-overlays)
  (save-excursion
    (goto-char (point-min))
    (let ((regexp (concat "\\_<" (regexp-opt dragonruby-concept-keywords) "\\_>")))
      (while (re-search-forward regexp nil t)
        (let ((concept (match-string 0))
              (start (match-beginning 0))
              (end (match-end 0)))
          ;; Check context: Ensure we are not inside a string or comment
          (let ((ppss (syntax-ppss)))
            (unless (or (nth 3 ppss) (nth 4 ppss))
              (dragonruby--make-concept-overlay start end concept))))))))

(defun dragonruby--after-concept-change (beg end _len)
  "Invalidate concepts in edited range and trigger rescanning."
  (save-match-data
    (remove-overlays beg end 'dragonruby-concept t))
  (dragonruby--debounce 'concepts #'dragonruby--scan-concepts 0.5))

(defun dragonruby--refresh-concepts ()
  (when (and (bound-and-true-p dragonruby-concepts-mode)
             (eq (current-buffer) (window-buffer)))
    (dragonruby--scan-concepts)))

(provide 'dragonruby-concept-visuals)
