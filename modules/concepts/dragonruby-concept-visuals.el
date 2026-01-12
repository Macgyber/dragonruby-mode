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
  "Action triggered when interacting with a concept.
First tries to open Org documentation with narrowing (Focused Learning).
Falls back to simple Knowledge Card buffer if Org unavailable."
  ;; Strategy: Try Org first (if docs module is loaded)
  (if (and (featurep 'dragonruby-docs)
           (fboundp 'dragonruby-docs-open-concept))
      (progn
        ;; Attempt Focused Learning via Org
        (dragonruby-docs-open-concept concept-name)
        ;; Check if it succeeded by looking for the hash-table entry
        (unless (and dragonruby-docs--concept-map
                     (gethash concept-name dragonruby-docs--concept-map))
          ;; Fallback: Org file not found, use Knowledge Card
          (dragonruby--show-knowledge-card concept-name)))
    ;; Docs module not loaded, use simple Knowledge Card
    (dragonruby--show-knowledge-card concept-name)))

(defun dragonruby--show-knowledge-card (concept-name)
  "Display a simple knowledge card for CONCEPT-NAME in a temporary buffer.
This is the fallback method when Org documentation is unavailable."
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

(defun dragonruby--normalize-concept (text)
  "Extract base concept from contextual usage.
Examples:
  'args.outputs' → 'outputs'
  '$gtk.state' → 'state'
  'outputs' → 'outputs'"
  (if (string-match "\\([a-z_]+\\)$" text)
      (match-string 1 text)
    text))

(defun dragonruby--scan-concepts ()
  "Scan buffer for DragonRuby concepts with contextual awareness.
Detects concepts in various forms:
  - Standalone: outputs, state, args
  - Property chain: args.outputs, args.state.player_x
  - Global toolkit: $gtk.outputs
  - Instance vars: @player.state"
  (dragonruby--clear-concept-overlays)
  (when dragonruby-concepts-debug
    (message "🧠 [Concepts] Contextual Scan Started in %s" (buffer-name)))
    
  (save-excursion
    (goto-char (point-min))
    ;; Build regex that matches concepts with optional prefix
    ;; Matches: word_boundary OR [.$@] followed by concept word
    (let ((concept-pattern (regexp-opt dragonruby-concept-keywords))
          (count 0))
      (while (re-search-forward 
              (concat "\\(?:\\(?:^\\|[^a-zA-Z0-9_]\\)\\(?:[$@]?[a-z_]+\\.\\)*\\)?"
                      "\\(" concept-pattern "\\)\\_>")
              nil t)
        (let* ((concept-match (match-string 1))  ; The actual concept word
               (start (match-beginning 1))
               (end (match-end 1))
               ;; Normalize to base concept (e.g., "outputs" from "args.outputs")
               (base-concept (dragonruby--normalize-concept concept-match)))
          
          ;; Check context: Ensure we are not inside a string or comment
          (let ((ppss (syntax-ppss start)))
            (unless (or (nth 3 ppss) (nth 4 ppss))
              (setq count (1+ count))
              (when dragonruby-concepts-debug
                (message "🧠 [Concepts] Found '%s' (base: %s) at %d" 
                         concept-match base-concept start))
              ;; Create overlay for the base concept
              (dragonruby--make-concept-overlay start end base-concept)))))
      
      (when dragonruby-concepts-debug
        (message "🧠 [Concepts] Scan Complete. Total contexts: %d" count)))))

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
