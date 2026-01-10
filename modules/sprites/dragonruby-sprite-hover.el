;;; dragonruby-sprite-hover.el --- Mouse hover detection for sprites -*- lexical-binding: t; -*-

(require 'dragonruby-sprite-popup)

(defvar-local dragonruby--last-hover-path nil
  "Path of last sprite we triggered popup for. Prevents repeat triggers.")

(defun dragonruby--sprite-check-hover ()
  "Check if mouse is over a sprite overlay and trigger popup once."
  (when (and (display-graphic-p)
             dragonruby-sprite-mode)
    (let* ((mpos (mouse-pixel-position))
           (frame (car-safe mpos))
           (x (cadr mpos))
           (y (let ((r (cddr mpos))) (and (consp r) (car r)))))
      (when (and (eq frame (selected-frame))
                 (numberp x) (numberp y))
        (let* ((posn (posn-at-x-y x y frame))
               (window (and posn (posn-window posn)))
               (point (and posn (posn-point posn))))
          (if (and (windowp window)
                   (eq (window-buffer window) (current-buffer))
                   (integerp point))
              ;; Mouse is in current buffer
              (let ((sprite-path nil))
                ;; Check if mouse is over a sprite overlay
                (dolist (ov (overlays-at point))
                  (when (overlay-get ov 'dragonruby-sprite)
                    (setq sprite-path (overlay-get ov 'dragonruby-sprite-path))))
                
                (if sprite-path
                    ;; Mouse over sprite - trigger if different from last
                    (unless (string-equal sprite-path dragonruby--last-hover-path)
                      (setq dragonruby--last-hover-path sprite-path)
                      (dragonruby--sprite-popup-trigger sprite-path))
                  ;; Mouse not over sprite - reset
                  (setq dragonruby--last-hover-path nil)))
            ;; Mouse not in current buffer - reset
            (setq dragonruby--last-hover-path nil)))))))

(provide 'dragonruby-sprite-hover)
;;; dragonruby-sprite-hover.el ends here
