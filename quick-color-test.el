;;; quick-color-test.el --- Quick test for color overlays -*- lexical-binding: t; -*-

;; Minimal test - can we create color overlays?

(defun quick-color-test ()
  "Quick test to see if color overlay creation works."
  (interactive)
  
  ;; Create a test buffer
  (with-current-buffer (get-buffer-create "*Quick Color Test*")
    (erase-buffer)
    (ruby-mode)  ; Use ruby-mode
    
    ;; Insert test content
    (insert "# Test colors\n")
    (insert "red = [255, 0, 0]\n")
    (insert "green = [0, 255, 0]\n")
    (insert "blue = [0, 0, 255]\n")
    
    ;; Now manually create ONE overlay to test if it works
    (goto-char (point-min))
    (when (re-search-forward "\\[255, 0, 0\\]" nil t)
      (let* ((start (match-beginning 0))
             (end (match-end 0))
             (ov (make-overlay start end)))
        
        ;; Apply RED background
       (overlay-put ov 'face '(:background "#ff0000" :foreground "white"))
        (overlay-put ov 'dragonruby-color-overlay t)
        
        (message "Created overlay at %d-%d" start end)
        (message "Overlay face: %s" (overlay-get ov 'face))))
    
    ;; Switch to buffer so user can see it
    (switch-to-buffer (current-buffer))
    (goto-char (point-min))))

;; Run immediately
(quick-color-test)
