;;; dragonruby-mode.el --- DragonRuby Minor Mode Implementation  -*- lexical-binding: t; -*-

;; 1. Core / UI Dependencies
(require 'dragonruby-colors)
(require 'dragonruby-sprites)
(require 'dragonruby-paths)

(defvar dragonruby-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") 'dragonruby-adjust-color-at-point)
    map)
  "Keymap for DragonRuby mode.")

(defun dragonruby--after-change (start end _len)
  "Hook to run all scanners on text change."
  (when dragonruby-mode
    ;; 1. Clean old overlays
    (dragonruby--clear-color-overlays start end)
    (dragonruby--clear-sprite-overlays start end)
    (dragonruby--clear-require-overlays start end)
    
    ;; 2. Scan window (context)
    (let ((s (max (point-min) (- start 150)))
          (e (min (point-max) (+ end 150))))
      (dragonruby--scan-colors-region s e)
      (dragonruby--scan-sprites-region s e)
      (dragonruby--scan-requires-region s e))))

(defun dragonruby--scan-all ()
  "Run full buffer scan."
  (dragonruby--scan-colors-entire-buffer)
  (dragonruby--scan-sprites-region (point-min) (point-max))
  (dragonruby--scan-requires-region (point-min) (point-max)))

;;;###autoload
(define-minor-mode dragonruby-mode
  "Minor mode for assisting DragonRuby development."
  :lighter " DR"
  :group 'dragonruby
  :keymap dragonruby-mode-map
  (if dragonruby-mode
      (progn
        ;; (dragonruby-navigation-enable) ;; Not used yet in this simplified version if it relied on eldoc hook
        (dragonruby--scan-all)
        (add-hook 'after-change-functions #'dragonruby--after-change nil t)
        (add-hook 'completion-at-point-functions #'dragonruby--completion-at-point nil t)
        ;; Optimizacion: Tooltip rapido (buffer-local)
        (set (make-local-variable 'tooltip-delay) dragonruby-tooltip-delay))
    ;; Disable
    ;; (dragonruby-navigation-disable)
    (remove-hook 'after-change-functions #'dragonruby--after-change t)
    (remove-hook 'completion-at-point-functions #'dragonruby--completion-at-point t)
    (kill-local-variable 'tooltip-delay)
    (remove-overlays (point-min) (point-max) 'dragonruby-color-overlay t)
    (remove-overlays (point-min) (point-max) 'dragonruby-sprite-overlay t)
    (remove-overlays (point-min) (point-max) 'dragonruby-require-overlay t)))

;; ---------------------------------------------------------------------------
;; Debug / Diagnostics
;; ---------------------------------------------------------------------------

(defun dragonruby-debug-status ()
  "Print diagnostic info about dragonruby-mode state."
  (interactive)
  (message "--- DragonRuby Debug ---")
  (message "Mode Active: %s" dragonruby-mode)
  (message "Project Root: %s" (or (funcall (intern-soft "dragonruby--project-root")) "NIL"))
  (message "Overlays in buffer: %d" (length (overlays-in (point-min) (point-max))))
  (message "Hooks active: %s" (memq #'dragonruby--after-change after-change-functions)))

;; ---------------------------------------------------------------------------
;; Ruby activation logic
;; ---------------------------------------------------------------------------

;;;###autoload
(defun dragonruby-mode-maybe-enable ()
  "Enable `dragonruby-mode` in DragonRuby Ruby files."
  (when (and (eq major-mode 'ruby-mode)
             (save-excursion
               (goto-char (point-min))
               (re-search-forward "def\\s-+tick\\s-*(args" nil t)))
    (dragonruby-mode 1)))

(add-hook 'ruby-mode-hook #'dragonruby-mode-maybe-enable)

(provide 'dragonruby-mode)
