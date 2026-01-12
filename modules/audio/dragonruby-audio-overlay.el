;;; dragonruby-audio-overlay.el --- Visual feedback for audio files -*- lexical-binding: t; -*-

(require 'dragonruby-core)

(defvar-local dragonruby--audio-overlays nil
  "List of audio overlays in the current buffer.")

(defun dragonruby--clear-audio-overlays ()
  "Remove all audio overlays."
  (remove-overlays (point-min) (point-max) 'dragonruby-audio t)
  (setq dragonruby--audio-overlays nil))

(defun dragonruby--make-audio-overlay (start end path valid)
  "Create an audio overlay for PATH."
  (let* ((ov (make-overlay start end))
         (ext (when path (file-name-extension path)))
         (info (cond (valid (format "Audio: %s\n(RET to open)" (file-name-nondirectory path)))
                     ((and ext (string-equal (downcase ext) "mp3")) "❌ MP3 not supported by DragonRuby (Use OGG/WAV)")
                     (t "❌ Audio file not found"))))
    (overlay-put ov 'dragonruby-audio t)
    (overlay-put ov 'help-echo info)
    
    (unless valid
      (overlay-put ov 'face '(:underline (:color "red" :style wave))))
    
    (when valid
      ;; Visual Feedback (Strictly OGG vs WAV)
      (let* ((ext-down (downcase (or ext "")))
             (label (cond ((string= ext-down "ogg") "📢")  ; Trompeta
                          ((string= ext-down "wav") "🔊"))) ; Altoparlante/Megáfono profesional
             (icon (when label (propertize (format " %s" label) 'face '(:height 0.9 :foreground "#2ECC71" :weight bold)))))
        (when icon
          (overlay-put ov 'after-string icon)))
      
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c C-o") (lambda () (interactive) (find-file path)))
        (define-key map (kbd "RET") (lambda () (interactive) (find-file path)))
        (overlay-put ov 'keymap map))
      (overlay-put ov 'mouse-face '(:background "#2ECC71" :foreground "black")))
    (push ov dragonruby--audio-overlays)))

(defun dragonruby--scan-audio-overlays ()
  "Scan buffer for audio references."
  (when dragonruby-mode
    (dragonruby--clear-audio-overlays)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (while (re-search-forward "['\"’“”]\\([^'\"\n!’“”]+\\.\\(wav\\|ogg\\|mp3\\)\\)['\"’“”]" nil t)
          (let* ((raw (match-string 1))
                 (start (match-beginning 0))
                 (end (match-end 0))
                 (abs (dragonruby--resolve-path raw 'audio))
                 (ext (downcase (or (file-name-extension raw) "")))
                 (supported (member ext '("wav" "ogg")))
                 (exists (and abs (file-exists-p abs)))
                 (valid (and supported (not (string-empty-p raw)) exists)))
            (dragonruby--make-audio-overlay start end abs valid)))))))

(provide 'dragonruby-audio-overlay)
;;; dragonruby-audio-overlay.el ends here
