;;; dragonruby-image-view.el --- Emacs buffer image viewing controls -*- lexical-binding: t; -*-

(require 'image-mode)

(defun dragonruby--goto-image ()
  "Move point to the first image in the buffer.
Returns t if an image was found, nil otherwise."
  (goto-char (point-min))
  (while (and (not (get-text-property (point) 'display))
              (not (eobp)))
    (forward-char 1))
  (and (get-text-property (point) 'display) t))

(defvar-local dragonruby--image-scale 1.0
  "Current scale factor for image zoom.")

(defun dragonruby-image-zoom-in ()
  "Zoom in the image."
  (interactive)
  (condition-case _err
      (when (dragonruby--goto-image)
        (setq dragonruby--image-scale (* dragonruby--image-scale 1.2))
        ;; Nearest neighbor scaling (pixel art mode)
        (image-transform-set-smoothing nil)
        (image-transform-set-scale dragonruby--image-scale))
    (error (message "⚠️ Zoom not available for this image type"))))

(defun dragonruby-image-zoom-out ()
  "Zoom out the image."
  (interactive)
  (condition-case _err
      (when (dragonruby--goto-image)
        (setq dragonruby--image-scale (/ dragonruby--image-scale 1.2))
        (image-transform-set-scale dragonruby--image-scale))
    (error (message "⚠️ Zoom not available for this image type"))))

(defun dragonruby-image-rotate ()
  "Rotate the image 90 degrees."
  (interactive)
  (when (dragonruby--goto-image)
    (image-rotate)))

(defun dragonruby-image-reset-zoom ()
  "Reset image by reloading the file completely."
  (interactive)
  (let ((file buffer-file-name))
    (when file
      (kill-buffer)
      (find-file file)
      (message "Image reloaded"))))

(provide 'dragonruby-image-view)
