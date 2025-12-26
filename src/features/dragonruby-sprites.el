;;; dragonruby-sprites.el --- Sprite previews -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar dragonruby--sprite-overlays nil)
(defvar dragonruby-supported-sprites '("png" "jpg" "jpeg" "gif"))
(defvar dragonruby-unsupported-sprites '("psd" "xcf"))

(defun dragonruby--clear-sprite-overlays ()
  (mapc #'delete-overlay dragonruby--sprite-overlays)
  (setq dragonruby--sprite-overlays nil))

(defun dragonruby--make-sprite-overlay (start end path real-path type)
  (let ((ov (make-overlay start end)))
    (if (eq type 'valid)
        (progn
          (overlay-put ov 'face '(:underline t))
          (overlay-put ov 'help-echo real-path)
          (overlay-put ov 'keymap 
                       (let ((map (make-sparse-keymap)))
                         (define-key map [mouse-1] (lambda () (interactive) (find-file real-path)))
                         map)))
      (overlay-put ov 'face `(:underline (:color ,(if (eq type 'missing) "red" "orange") :style wave)))
      (overlay-put ov 'help-echo (if (eq type 'missing) "File not found" "Format unsafe")))
    
    (push ov dragonruby--sprite-overlays)))

(defun dragonruby--scan-sprites ()
  (dragonruby--clear-sprite-overlays)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\"\\([^\"\n]+\\)\"" nil t)
      (let* ((start (match-beginning 1))
             (end (match-end 1))
             (path (match-string 1))
             (ext (file-name-extension path)))
        (when ext
          (setq ext (downcase ext))
          (cond
           ((member ext dragonruby-supported-sprites)
            (if (file-exists-p path)
                (dragonruby--make-sprite-overlay start end path path 'valid)
              (dragonruby--make-sprite-overlay start end path nil 'missing)))
           ((member ext dragonruby-unsupported-sprites)
            (dragonruby--make-sprite-overlay start end path nil 'unsupported))))))))

(defun dragonruby--after-sprite-change (_beg _end _len)
  (dragonruby--scan-sprites))

(define-minor-mode dragonruby-sprite-mode
  "Sprite previews."
  :lighter " DR-Sprite"
  (if dragonruby-sprite-mode
      (progn
        (add-hook 'after-change-functions #'dragonruby--after-sprite-change nil t)
        (dragonruby--scan-sprites))
    (remove-hook 'after-change-functions #'dragonruby--after-sprite-change t)
    (dragonruby--clear-sprite-overlays)))

(provide 'dragonruby-sprites)
