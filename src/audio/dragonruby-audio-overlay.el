;;; dragonruby-audio-overlay.el --- Visual feedback for audio files -*- lexical-binding: t; -*-

(require 'dragonruby-core)
(require 'dragonruby-assets)

(defvar-local dragonruby--audio-overlays nil
  "List of audio overlays in the current buffer.")

(defun dragonruby--clear-audio-overlays ()
  "Remove all audio overlays."
  (remove-overlays (point-min) (point-max) 'dragonruby-audio t)
  (setq dragonruby--audio-overlays nil))

(defun dragonruby--make-audio-overlay (start end path valid)
  "Create an audio overlay for PATH."
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'face (if valid 
                             '(:underline (:color "#2ECC71" :style wave)) 
                           '(:underline (:color "red" :style wave))))
    (overlay-put ov 'help-echo (cond (valid 
                                     (let ((size (float (nth 7 (file-attributes path)))))
                                       (format "Audio: %s\nSize: %s\n(RET or Click to open)" 
                                               (file-name-nondirectory path)
                                               (cond ((>= size 1048576) (format "%.2f MB" (/ size 1048576.0)))
                                                     ((>= size 1024) (format "%.2f KB" (/ size 1024.0)))
                                                     (t (format "%d bytes" (truncate size)))))))
                                   ((string= (file-name-extension path) "mp3") "❌ MP3 not supported by DragonRuby (Use OGG/WAV)")
                                   (t "❌ Audio not found")))
    (overlay-put ov 'dragonruby-audio t)
    (when valid
      ;; Visual Feedback (Strictly OGG vs WAV)
      (let* ((ext (file-name-extension path))
             (icon (cond ((string= ext "ogg") "  🔊")
                         ((string= ext "wav") "  📢"))))
        (when icon
          (overlay-put ov 'after-string (propertize icon 'face '(:height 0.9 :foreground "#2ECC71")))))
      
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c C-o") (lambda () (interactive) (find-file path)))
        (define-key map (kbd "RET") (lambda () (interactive) (find-file path)))
        (overlay-put ov 'keymap map))
      (overlay-put ov 'mouse-face '(:background "#2ECC71" :foreground "black")))
    (push ov dragonruby--audio-overlays)))

(defun dragonruby--scan-audio-overlays ()
  "Scan buffer for audio references."
  (when (and dragonruby-mode (bound-and-true-p dragonruby-audio-mode))
    (dragonruby--clear-audio-overlays)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward "\"\\([^\"\n!]+\\.\\(wav\\|ogg\\|mp3\\)\\)\"" nil t)
          (let* ((raw (match-string 1))
                 (start (match-beginning 0))
                 (end (match-end 0))
                 (abs (dragonruby--resolve-path raw 'audio))
                 (ext (file-name-extension raw))
                 (supported (member ext '("wav" "ogg")))
                 (valid (and supported (not (string-empty-p raw)) (file-exists-p abs))))
            (dragonruby--make-audio-overlay start end abs valid)))))))

(provide 'dragonruby-audio-overlay)
;;; dragonruby-audio-overlay.el ends here
