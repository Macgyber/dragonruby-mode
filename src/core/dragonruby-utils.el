;;; dragonruby-utils.el --- Generic utility functions -*- lexical-binding: t; -*-

(defun dragonruby--files-in (dir extensions)
  "List files recursively in DIR matching EXTENSIONS."
  (let ((pattern (concat "\\." (regexp-opt extensions) "$")))
    (when (and dir (file-directory-p dir))
      (directory-files-recursively dir pattern))))

;; --- DEBOUNCE UTILITY ---
(defvar-local dragonruby--debounce-timers nil
  "Registry of timers for different debounced tasks.")

(defun dragonruby--debounce (task-id func delay)
  "Run FUNC after DELAY seconds of idleness for TASK-ID.
Uses Emacs idle timers to ensure we wait for the user to 'breathe'."
  (unless (hash-table-p dragonruby--debounce-timers)
    (setq dragonruby--debounce-timers (make-hash-table :test 'eq)))
  (let ((buf (current-buffer))
        (timer (gethash task-id dragonruby--debounce-timers)))
    (when timer (cancel-timer timer))
    (puthash task-id
             (run-with-idle-timer delay nil
                                  (lambda ()
                                    (when (buffer-live-p buf)
                                      (with-current-buffer buf
                                        (save-match-data
                                          (condition-case err
                                              (progn
                                                (funcall func)
                                                ;; Optimization: Redisplay only if visible
                                                (when (get-buffer-window buf)
                                                  (redisplay)))
                                            (error (message "DragonRuby Debounce Error [%s]: %S" task-id err))))))))
             dragonruby--debounce-timers)))

(provide 'dragonruby-utils)
