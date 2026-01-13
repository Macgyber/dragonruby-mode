;;; dragonruby-utils.el --- Generic utility functions -*- lexical-binding: t; -*-

;; --- PROJECT ROOT FINDER ---

(defvar-local dragonruby--project-root-cache nil
  "Cache for the project root to avoid constant disk access.")

(defvar-local dragonruby--project-root-last-dir nil
  "Last directory used to calculate the project root.")

(defun dragonruby--find-project-root ()
  "Locate the DragonRuby project root directory safely.
Invalidates cache if `default-directory` changes to ensure multi-project robustness."
  (unless (string= default-directory dragonruby--project-root-last-dir)
    (setq-local dragonruby--project-root-cache nil)
    (setq-local dragonruby--project-root-last-dir default-directory))

  (let ((db-root (or dragonruby--project-root-cache
                     (let ((found (locate-dominating-file default-directory
                                           (lambda (dir)
                                             (let ((p1 (expand-file-name "dragonruby" dir))
                                                   (p2 (expand-file-name "dragonruby.exe" dir))
                                                   (p3 (expand-file-name "app/main.rb" dir))
                                                   ;; Fallback for Plugin Development
                                                   (p4 (expand-file-name ".git" dir))
                                                   (p5 (expand-file-name "dragonruby-mode.el" dir)))
                                               (or (file-exists-p p1)
                                                   (file-exists-p p2)
                                                   (file-exists-p p3)
                                                   (file-exists-p p4)
                                                   (file-exists-p p5)))))))
                       (when found
                         ;; If we found a sub-root (like mygame/), check if the parent is the Master Root (binary)
                         (let ((parent (file-name-directory (directory-file-name found))))
                           (if (and parent 
                                    (or (file-exists-p (expand-file-name "dragonruby" parent))
                                        (file-exists-p (expand-file-name "dragonruby.exe" parent))))
                               parent
                             found)))))))
    (when db-root
      (setq-local dragonruby--project-root-cache (expand-file-name db-root)))
    dragonruby--project-root-cache))

(defun dragonruby--cache-dir (subdir)
  "Return a safe cache directory path in .emacs.d/dragonruby/SUBDIR.
Ensures the directory exists."
  (let ((dir (expand-file-name (concat "dragonruby/" subdir) user-emacs-directory)))
    (unless (file-exists-p dir)
      (make-directory dir t))
    dir))

(defun dragonruby--files-in (dir extensions)
  "List files recursively in DIR matching EXTENSIONS.
Returns paths relative to DIR."
  (let ((pattern (concat "\\." (regexp-opt extensions) "$")))
    (when (and dir (file-directory-p dir))
      (mapcar (lambda (file) (file-relative-name file dir))
              (directory-files-recursively dir pattern)))))

(defun dragonruby--collect-project-files (&optional type)
  "Collect all files of TYPE in the project root.
If TYPE is nil, collect ALL supported files."
  (let* ((root (dragonruby--find-project-root))
         (exts (cond
                ((eq type 'sprite) dragonruby-image-exts)
                ((eq type 'audio) dragonruby-audio-exts)
                ((eq type 'font) dragonruby-font-exts)
                ((eq type 'data) dragonruby-data-exts)
                ((eq type 'ruby) dragonruby-code-exts)
                ((null type) (append dragonruby-image-exts 
                                     dragonruby-audio-exts 
                                     dragonruby-font-exts 
                                     dragonruby-data-exts 
                                     dragonruby-code-exts))
                (t nil))))
    (when (and root exts)
      (dragonruby--files-in root exts))))

;; --- DEBOUNCE UTILITY ---

(defvar dragonruby--debounce-timers (make-hash-table :test 'eq)
  "Store active timers for debounce IDs.")

(defun dragonruby--debounce (id func delay)
  "Run FUNC after DELAY seconds, coalescing calls with same ID.
Ensures hygiene by removing the timer from registry after execution."
  (let ((old-timer (gethash id dragonruby--debounce-timers)))
    (when old-timer (cancel-timer old-timer))
    (puthash id
             (dragonruby-kernel-register-timer
              (run-with-timer delay nil
                               (lambda ()
                                 (remhash id dragonruby--debounce-timers)
                                 (funcall func))))
             dragonruby--debounce-timers)))

(defvar-local dragonruby--notified-messages nil
  "List of messages already shown in this buffer.")

(defun dragonruby--notify (id msg &optional once)
  "Show MSG in the echo area. If ONCE is t, only show it once per session."
  (unless (and once (member id dragonruby--notified-messages))
    (message msg)
    (when once (push id dragonruby--notified-messages))))

(defvar-local dragonruby--warned-features nil
  "List of features that have already shown a development warning.")

(defun dragonruby--warn-in-development (feature)
  "Show a brief warning that FEATURE is still in development.
The warning is only shown once per session to avoid annoyance."
  (interactive)
  (unless (member feature dragonruby--warned-features)
    (let ((msg (format "【 🚧 DragonRuby 】\n\n'%s' is still under DEVELOPMENT.\n\nDismiss notice?" feature)))
      (read-multiple-choice msg
                            '((?c "Close" "Dismiss notice")))
      (push feature dragonruby--warned-features))))

(defun dragonruby--resolve-path (raw-path type-or-extensions &optional allow-examples)
  "Resolve RAW-PATH based on TYPE-OR-EXTENSIONS.
If 2nd arg is a LIST, treat as allowed extensions (legacy behavior).
If 2nd arg is a SYMBOL, treat as semantic type ('require, 'sprite, etc).
If ALLOW-EXAMPLES is non-nil, attempt fallback to examples/ folder."
  (let* ((root (dragonruby--find-project-root))
         (current-dir (file-name-directory (or buffer-file-name default-directory))))
    
    (if (listp type-or-extensions)
        ;; LEGACY MODE: (path extensions-list)
        (let ((clean-path (string-trim (string-trim raw-path "['\"]") "['\"]")))
          (when (and root (not (string-empty-p clean-path)))
            (let* ((abs-path (expand-file-name clean-path root))
                   (ext (file-name-extension abs-path)))
              (when (and (file-exists-p abs-path)
                         (or (null type-or-extensions)
                             (and ext (member (downcase ext) type-or-extensions))))
                abs-path))))
      
      ;; SMART MODE: (path type-symbol)
      (let* ((type type-or-extensions)
             (base-dir (cond
                        ((eq type 'require_relative) current-dir)
                        ((memq type '(sprite audio font data)) (or root current-dir))
                        (t (or root current-dir))))
             (ensure-rb (memq type '(require require_relative)))
             (path (if (and ensure-rb (not (string-suffix-p ".rb" raw-path)))
                       (concat raw-path ".rb")
                     raw-path))
             (full-path (and path base-dir (expand-file-name path base-dir))))
        
        (cond
         ((and full-path 
               (file-exists-p full-path)
               (not (file-directory-p full-path)))
          full-path)
         ;; OPT-IN FALLBACK: Try examples/ if explicitly allowed
         ((and allow-examples root (not (string-prefix-p "examples/" raw-path)))
          (let ((ex-path (expand-file-name (concat "examples/" raw-path) root)))
            (if (and (file-exists-p ex-path) (not (file-directory-p ex-path)))
                ex-path
              nil)))
         (t nil))))))

(defun dragonruby--visible-region ()
  "Return (START . END) of the visible portion of the current window.
Includes a small margin (padding) to make scrolling smoother."
  (if (get-buffer-window (current-buffer))
      (let* ((start (window-start))
             (end (window-end nil t))
             (padding 3000))
        (cons (max (point-min) (- start padding))
              (min (point-max) (+ end padding))))
    (cons (point-min) (point-max))))

(defun dragonruby--get-image-type (path)
  "Guess Emacs image type symbol from PATH extension."
  (let ((ext (and path (downcase (file-name-extension path)))))
    (cond ((member ext '("png")) 'png)
          ((member ext '("jpg" "jpeg")) 'jpeg)
          ((member ext '("gif")) 'gif)
          ((member ext '("bmp")) 'bmp)
          ((member ext '("webp")) 'webp)
          ((member ext '("svg")) 'svg)
          (t nil))))

(defun dragonruby-diagnose-imagemagick ()
  "Check if ImageMagick is correctly configured and accessible."
  (interactive)
  (let ((cmd (or (executable-find "magick") (executable-find "convert"))))
    (if cmd
        (message "✅ SUCCESS: ImageMagick found at: %s" cmd)
      (message "❌ FAILURE: ImageMagick NOT found in exec-path.\nValue of exec-path: %s" exec-path))))

(defun dragonruby-diagnose ()
  "Show complete diagnostic information about DragonRuby Mode."
  (interactive)
  (let ((buf (get-buffer-create "*DragonRuby Diagnostics*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "🐲 DragonRuby Mode Diagnostics\n")
      (insert "================================\n\n")
      
      ;; Mode status
      (insert (format "dragonruby-mode: %s\n" (if (bound-and-true-p dragonruby-mode) "✅ ACTIVE" "❌ INACTIVE")))
      
      ;; Scan hook
      (insert (format "\ndragonruby-scan-hook functions:\n"))
      (if dragonruby-scan-hook
          (dolist (fn dragonruby-scan-hook)
            (insert (format "  - %s\n" fn)))
        (insert "  (empty - no scanners registered!)\n"))
      
      ;; ImageMagick
      (insert (format "\nImageMagick: %s\n" 
                      (if (or (executable-find "magick") (executable-find "convert"))
                          "✅ FOUND" "❌ NOT FOUND")))
      
      ;; Project root
      (insert (format "\nProject Root: %s\n" (or (dragonruby--find-project-root) "NOT FOUND")))
      
      (insert "\n================================\n")
      (insert "Run M-x dragonruby-mode to toggle the mode.\n"))
    (display-buffer buf)))

(provide 'dragonruby-utils)
