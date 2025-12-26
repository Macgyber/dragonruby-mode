;;; dragonruby-paths.el --- Path navigation -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar dragonruby--path-overlays nil)
(defvar dragonruby-supported-paths '("rb" "json" "ogg" "wav" "mp3"))

(defun dragonruby--clear-path-overlays ()
  (mapc #'delete-overlay dragonruby--path-overlays)
  (setq dragonruby--path-overlays nil))

(defun dragonruby--make-path-overlay (start end path found)
  (let ((ov (make-overlay start end)))
    (if found
        (progn
          (overlay-put ov 'face '(:underline t))
          (overlay-put ov 'help-echo path)
          (overlay-put ov 'keymap 
                       (let ((map (make-sparse-keymap)))
                         (define-key map [mouse-1] (lambda () (interactive) (find-file path)))
                         map)))
      (overlay-put ov 'face '(:underline (:color "red" :style wave)))
      (overlay-put ov 'help-echo "Path not found"))
    (push ov dragonruby--path-overlays)))

(defun dragonruby--scan-paths ()
  (dragonruby--clear-path-overlays)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\"\\([^\"\n]+\\)\"" nil t)
      (let* ((start (match-beginning 1))
             (end (match-end 1))
             (path (match-string 1))
             (ext (file-name-extension path)))
        (when (and ext (member (downcase ext) dragonruby-supported-paths))
          (dragonruby--make-path-overlay start end path (file-exists-p path)))))))

(defun dragonruby--after-path-change (_beg _end _len)
  (dragonruby--scan-paths))

(define-minor-mode dragonruby-paths-mode
  "Path navigation."
  :lighter " DR-Path"
  (if dragonruby-paths-mode
      (progn
        (add-hook 'after-change-functions #'dragonruby--after-path-change nil t)
        (dragonruby--scan-paths))
    (remove-hook 'after-change-functions #'dragonruby--after-path-change t)
    (dragonruby--clear-path-overlays)))

(provide 'dragonruby-paths)
