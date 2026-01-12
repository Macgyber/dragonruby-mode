;;; dragonruby-audio-fs.el --- Filesystem operations for audio -*- lexical-binding: t; -*-

(require 'dragonruby-core)

(defvar dragonruby--audio-duration-cache (make-hash-table :test 'equal)
  "Cache for audio durations to avoid repeated shell calls.")

(defun dragonruby--audio-get-duration (path)
  "Get duration of audio file at PATH.
Uses native Mac tools (mdls, afinfo) and caches results."
  (let* ((attrs (file-attributes path))
         (mtime (format-time-string "%Y%m%d%H%M%S" (file-attribute-modification-time attrs)))
         (cache-key (concat path "-" mtime))
         (cached (gethash cache-key dragonruby--audio-duration-cache)))
    (if cached
        cached
      (let ((duration nil))
        ;; 1. Try mdls (Blazing fast Mac metadata)
        (let ((mdls-out (shell-command-to-string (format "mdls -name kMDItemDurationSeconds -raw %s" (shell-quote-argument path)))))
          (when (and mdls-out (string-match-p "^[0-9.]+$" mdls-out))
            (setq duration (string-to-number mdls-out))))
        
        ;; 2. Fallback to afinfo -r (Forces scanning for real duration, better for OGG)
        (when (not duration)
          (let ((af-out (shell-command-to-string (format "afinfo -r %s | grep 'duration:' | awk '{print $3}'" (shell-quote-argument path)))))
            (when (and af-out (string-match-p "^[0-9.]+$" af-out))
              (setq duration (string-to-number af-out)))))

        ;; Format and Cache
        (let ((result (if duration
                          (let ((mins (/ (truncate duration) 60))
                                (secs (% (truncate duration) 60)))
                            (format "%d:%02d" mins secs))
                        "--:--")))
          (puthash cache-key result dragonruby--audio-duration-cache)
          result)))))

(defun dragonruby--get-all-sounds-in-project ()
  "Get all sound files in the project."
  (dragonruby--collect-project-files 'audio))

(provide 'dragonruby-audio-fs)
;;; dragonruby-audio-fs.el ends here
