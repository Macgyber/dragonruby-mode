;;; dragonruby-audio-overlay.el --- Visual feedback for audio files -*- lexical-binding: t; -*-

(require 'dragonruby-core)

(defvar-local dragonruby--audio-overlays nil
  "List of audio overlays in the current buffer.")

(defun dragonruby--clear-audio-overlays ()
  "Remove all audio overlays."
  (remove-overlays (point-min) (point-max) 'dragonruby-audio t)
  (setq dragonruby--audio-overlays nil))

(defun dragonruby--make-audio-overlay (start end path valid)
  "Create an audio overlay for PATH with rich metadata metadata."
  (let* ((ov (make-overlay start end))
         (ext (when path (file-name-extension path)))
         (attrs (when (and path (file-exists-p path)) (file-attributes path)))
         (size (if attrs (file-size-human-readable (file-attribute-size attrs)) "?"))
         (duration (if valid (dragonruby--audio-get-duration path) "--:--"))
         (info (cond (valid 
                      (let ((type-str (upcase (or ext "AUDIO"))))
                        (format "⏱ %s | 📏 %s | 🏷 %s\n\n(RET para abrir)" 
                                duration size type-str)))
                     ((and ext (string-equal (downcase ext) "mp3")) 
                      "❌ MP3 no soportado (Usa OGG/WAV)")
                     (t "❌ Archivo no encontrado"))))
    (overlay-put ov 'dragonruby-audio t)
    (overlay-put ov 'help-echo (propertize info 'face 'bold))
    
    (unless valid
      (overlay-put ov 'face '(:underline (:color "red" :style wave))))
    
    (when valid
      ;; Visual Feedback (Strictly OGG vs WAV)
      (let* ((ext-down (downcase (or ext "")))
             (label (cond ((string= ext-down "ogg") "📢")
                          ((string= ext-down "wav") "🔊")))
             (icon (when label (propertize (format " %s" label) 'face '(:height 0.9 :foreground "#2ECC71" :weight bold)))))
        (when icon
          (overlay-put ov 'after-string icon)))
      
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c C-o") (lambda () (interactive) (find-file path)))
        (define-key map (kbd "RET") (lambda () (interactive) (find-file path)))
        (overlay-put ov 'keymap map))
      (overlay-put ov 'mouse-face '(:background "#2ECC71" :foreground "black")))
    (push ov dragonruby--audio-overlays)))

(defun dragonruby--scan-audio-overlays (&optional beg end)
  "Scan VISIBLE region for audio references.
Passes BEG and END as provided by central scheduler."
  (when dragonruby-mode
    (let ((start-pos (or beg (point-min)))
          (end-pos (or end (point-max))))
      (remove-overlays start-pos end-pos 'dragonruby-audio t)
      (save-excursion
        (goto-char start-pos)
        (while (re-search-forward "['\"’“”]\\([^'\"\n!’“”]+\\.\\(wav\\|ogg\\|mp3\\)\\)['\"’“”]" end-pos t)
          (let* ((raw (match-string 1))
                 (abs (dragonruby--resolve-path raw 'audio))
                 (ext (downcase (or (file-name-extension raw) "")))
                 (supported (member ext '("wav" "ogg")))
                 (attrs (and abs (file-attributes abs)))
                 (exists (and attrs (not (file-directory-p abs))))
                 (size (if attrs (file-attribute-size attrs) 0))
                 (valid (and supported (not (string-empty-p raw)) exists (> size 0))))
            (dragonruby--make-audio-overlay (match-beginning 0) (match-end 0) abs valid)))))))

(provide 'dragonruby-audio-overlay)
;;; dragonruby-audio-overlay.el ends here
