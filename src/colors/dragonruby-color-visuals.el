;;; dragonruby-color-visuals.el --- Overlays for color visualization -*- lexical-binding: t; -*-

(require 'dragonruby-color-utils)

(declare-function dragonruby-color-picker-edit "dragonruby-color-picker" (start end format))

(defvar-local dragonruby--color-overlays nil
  "List of active color overlays in the current buffer. (Legacy tracker)")

(defcustom dragonruby-colors-overlay-priority -50
  "Overlay priority for dragonruby color blocks."
  :type 'integer
  :group 'dragonruby-colors)

(defun dragonruby--clear-color-overlays ()
  "Remove color overlays using the \\='dragonruby-color property."
  (remove-overlays (point-min) (point-max) 'dragonruby-color t)
  (setq dragonruby--color-overlays nil))

(defun dragonruby--make-color-overlay (start end r g b &optional a format)
  "Create a color overlay for the range [START, END].
R, G, B, A are color components (0-255). FORMAT is ignored."
  (ignore format)
  (when (dragonruby--valid-rgba-p r g b a)
    (cl-destructuring-bind (rr gg bb)
        (dragonruby--apply-alpha r g b a)
      (let* ((hex (dragonruby--rgb-to-hex rr gg bb))
             (contrast (dragonruby--get-contrast-color rr gg bb))
             (ov (make-overlay start end)))
        (overlay-put ov 'face
                     `(:background ,hex
                       :foreground ,contrast
                       :box (:line-width -1 :color "grey50")))
        (overlay-put ov 'priority dragonruby-colors-overlay-priority)
        (overlay-put ov 'dragonruby-color t)
        (overlay-put ov 'help-echo "Click to edit color")
        (overlay-put ov 'keymap
                     (let ((map (make-sparse-keymap)))
                       (define-key map [mouse-1]
                         (lambda () (interactive)
                           (if (bound-and-true-p dragonruby-enable-picker)
                               (dragonruby-color-picker-edit start end nil) ;; format nil for now
                             (message "Interactive color picker not enabled"))))
                       map))
        (push ov dragonruby--color-overlays)))))

(provide 'dragonruby-color-visuals)
;;; dragonruby-color-visuals.el ends here
