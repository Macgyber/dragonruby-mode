;;; test-colors-debug.el --- Debug color overlay system -*- lexical-binding: t; -*-

;; This file helps debug why color overlays are not showing

;; Load paths
(add-to-list 'load-path (expand-file-name "src"))
(add-to-list 'load-path (expand-file-name "src/core"))
(add-to-list 'load-path (expand-file-name "src/ui"))

;; Load required modules
(require 'dragonruby-config)
(require 'dragonruby-colors-ui)

(defun test-color-overlay ()
  "Test if color overlays can be created."
  (interactive)
  (with-current-buffer (get-buffer-create "*Color Test*")
    (erase-buffer)
    (insert "Testing color overlays:\n\n")
    (insert "Red:   [255, 0, 0]\n")
    (insert "Green: [0, 255, 0]\n")
    (insert "Blue:  [0, 0, 255]\n")
    (insert "Alpha: [128, 128, 128, 200]\n")
    (insert "Meta:  [255, 128, 64, 255, 10]\n\n")
    
    ;; Show config
    (insert (format "Config - dragonruby-enable-color-preview: %s\n" 
                    dragonruby-enable-color-preview))
    (insert (format "Config - dragonruby-max-overlays-per-type: %s\n\n" 
                    dragonruby-max-overlays-per-type))
    
    ;; Try to create overlays manually
    (goto-char (point-min))
    (dragonruby--scan-colors-region (point-min) (point-max))
    
    ;; Count overlays created
    (let ((count 0))
      (dolist (ov (overlays-in (point-min) (point-max)))
        (when (overlay-get ov 'dragonruby-color-overlay)
          (setq count (1+ count))
          (insert (format "Overlay found at %d-%d\n" 
                         (overlay-start ov) 
                         (overlay-end ov)))))
      
      (insert (format "\nTotal color overlays created: %d\n" count))
      (if (= count 0)
          (insert "\n❌ NO OVERLAYS CREATED!\n")
        (insert "\n✅ Overlays successfully created!\n")))
    
    (switch-to-buffer (current-buffer))
    (goto-char (point-min))))

(message "")
(message "========================================")
(message "Color Overlay Debug Loaded")
(message "========================================")
(message "")
(message "Run: M-x test-color-overlay")
(message "")
(message "Or run in batch mode:")
(message "  (test-color-overlay)")
(message "")

;; Auto-run in batch mode
(when noninteractive
  (test-color-overlay)
  (princ (buffer-string)))
