;;; dragonruby-concept-visuals.el --- Visual overlays for concepts -*- lexical-binding: t; -*-

(require 'dragonruby-core)
(require 'dragonruby-guide)
(require 'dragonruby-knowledge)

(defvar dragonruby-concept-keywords
  '("args" "outputs" "inputs" "state" "grid" "geometry" "tick" "sprites" "solids" "borders" "labels")
  "List of DragonRuby concepts to highlight.")

(defvar-local dragonruby--concept-overlays nil
  "List of active DragonRuby concept overlays in the current buffer.")

;; -------------------------
;; Overlay lifecycle
;; -------------------------

(defun dragonruby--clear-concept-overlays (&optional start end)
  "Remove concept overlays from specified range [START, END]."
  (setq dragonruby--concept-overlays
        (cl-remove-if
         (lambda (ov)
           (when (overlayp ov)
             (let ((pos (overlay-start ov)))
               (when (and pos
                          (>= pos (or start (point-min)))
                          (<= pos (or end (point-max))))
                 (delete-overlay ov)
                 t))))
         dragonruby--concept-overlays)))

;; -------------------------
;; Concept activation
;; -------------------------

(defun dragonruby--activate-concept-at-point ()
  "Activate the DragonRuby concept under point."
  (interactive)
  (let ((ov (cl-find-if (lambda (o) (overlay-get o 'dragonruby-concept))
                       (overlays-at (point)))))
    (when ov
      (let ((concept (overlay-get ov 'dragonruby-concept-id)))
        (if (and (featurep 'dragonruby-guide)
                 (fboundp 'dragonruby-guide-open-concept))
            (dragonruby-guide-open-concept concept)
          (message "📖 Guide module not loaded for '%s'" concept))))))

;; Shared keymap (NO closures)
(defvar dragonruby--concept-overlay-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'dragonruby--activate-concept-at-point)
    (define-key map (kbd "C-c C-o") #'dragonruby--activate-concept-at-point)
    map)
  "Keymap used by DragonRuby concept overlays.")

;; -------------------------
;; Overlay creation
;; -------------------------

(defun dragonruby--make-concept-overlay (start end concept-name)
  "Create an overlay for CONCEPT-NAME."
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'face '(:underline (:color "#61AFEF" :style wave)))
    (overlay-put ov 'help-echo (format "🧠 %s (Press RET to learn more)" (upcase concept-name)))
    (overlay-put ov 'dragonruby-concept t)
    (overlay-put ov 'dragonruby-concept-id concept-name)
    (overlay-put ov 'priority 50)
    (overlay-put ov 'keymap dragonruby--concept-overlay-map)
    (push ov dragonruby--concept-overlays)))

;; -------------------------
;; Scanner
;; -------------------------

(defun dragonruby--scan-concepts ()
  "Scan visible region for DragonRuby concepts."
  (when (and (bound-and-true-p dragonruby-mode)
             (bound-and-true-p dragonruby-enable-concepts))
    (let* ((region (dragonruby--visible-region))
           (start-pos (car region))
           (end-pos (cdr region))
           (pattern (regexp-opt dragonruby-concept-keywords 'symbols)))

      (dragonruby--clear-concept-overlays start-pos end-pos)

      (save-excursion
        (goto-char start-pos)
        (while (re-search-forward pattern end-pos t)
          (when (dragonruby--in-code-p)
            (dragonruby--make-concept-overlay
             (match-beginning 0)
             (match-end 0)
             (match-string 0))))))))

(provide 'dragonruby-concept-visuals)
