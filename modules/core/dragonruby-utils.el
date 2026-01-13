;;; dragonruby-utils.el --- Generic utility functions -*- lexical-binding: t; -*-

;; --- PROJECT ROOT FINDER ---

(defvar-local dragonruby--project-root-cache nil
  "Cache for the project root to avoid constant disk access.")

(defvar-local dragonruby--project-root-last-dir nil
  "Last directory used to calculate the project root.")

(defvar dragonruby--global-project-root nil
  "Last confirmed valid project root (Global fallback).")

(defun dragonruby--find-project-root ()
  "Locate the DragonRuby project root directory safely.
Prioritizes the folder containing the 'dragonruby' binary (The Master Root)."
  (unless (string= default-directory dragonruby--project-root-last-dir)
    (setq-local dragonruby--project-root-cache nil)
    (setq-local dragonruby--project-root-last-dir default-directory))

  (let ((db-root (or dragonruby--project-root-cache
                     (let* ((search-dir default-directory)
                            ;; 1. Absolute Priority: The folder with the Binary
                            (binary-root (locate-dominating-file search-dir
                                           (lambda (dir)
                                             (or (file-exists-p (expand-file-name "dragonruby" dir))
                                                 (file-exists-p (expand-file-name "dragonruby-macos" dir))
                                                 (file-exists-p (expand-file-name "dragonruby.exe" dir))))))
                            ;; 2. Sub-Root Fallback: The folder with the app code
                            (content-root (locate-dominating-file search-dir
                                            (lambda (dir)
                                              (or (file-exists-p (expand-file-name "app/main.rb" dir))
                                                  (file-exists-p (expand-file-name "mygame" dir))))))
                            ;; 3. Development Fallback: Plugin folders
                            (dev-root (locate-dominating-file search-dir
                                        (lambda (dir)
                                          (or (file-exists-p (expand-file-name ".git" dir))
                                              (file-exists-p (expand-file-name "dragonruby-mode.el" dir)))))))
                       
                       (or binary-root content-root dev-root)))))
    (when db-root
      (let ((abs-root (expand-file-name db-root)))
        (setq-local dragonruby--project-root-cache abs-root)
        ;; If it's a real project root (contains binary or app), update global fallback
        (when (or (file-exists-p (expand-file-name "dragonruby" abs-root))
                  (file-exists-p (expand-file-name "app/main.rb" abs-root)))
          (setq dragonruby--global-project-root abs-root))))
    
    ;; Use local cache followed by global fallback
    (or db-root dragonruby--global-project-root)))

(defun dragonruby--cache-dir (subdir)
  "Return a safe cache directory path in .emacs.d/dragonruby/SUBDIR.
Ensures the directory exists."
  (let ((dir (expand-file-name (concat "dragonruby/" subdir) user-emacs-directory)))
    (unless (file-exists-p dir)
      (make-directory dir t))
    dir))

(defun dragonruby--files-in (dir extensions)
  "List files recursively in DIR matching EXTENSIONS."
  (let ((pattern (concat "\\." (regexp-opt extensions) "$")))
    (when (and dir (file-directory-p dir))
      (mapcar (lambda (file) (file-relative-name file dir))
              (directory-files-recursively dir pattern)))))

(defun dragonruby--collect-project-files (&optional type)
  "Collect all files of TYPE in the project root."
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

(defvar dragonruby--debounce-timers (make-hash-table :test 'eq))

(defun dragonruby--debounce (id func delay)
  "Run FUNC after DELAY seconds, coalescing calls."
  (let ((old-timer (gethash id dragonruby--debounce-timers)))
    (when old-timer (cancel-timer old-timer))
    (puthash id
             (dragonruby-kernel-register-timer
              (run-with-timer delay nil
                              (lambda ()
                                (remhash id dragonruby--debounce-timers)
                                (funcall func))))
             dragonruby--debounce-timers)))

(defvar-local dragonruby--notified-messages nil)

(defun dragonruby--notify (id msg &optional once)
  "Show MSG in the echo area."
  (unless (and once (member id dragonruby--notified-messages))
    (message msg)
    (when once (push id dragonruby--notified-messages))))

(defvar-local dragonruby--warned-features nil)

(defun dragonruby--warn-in-development (feature)
  "Show a dev warning for FEATURE."
  (interactive)
  (unless (member feature dragonruby--warned-features)
    (let ((msg (format "【 🚧 DragonRuby 】\n\n'%s' is still under DEVELOPMENT.\n\nDismiss notice?" feature)))
      (read-multiple-choice msg '((?c "Close" "Dismiss notice")))
      (push feature dragonruby--warned-features))))

(defun dragonruby--resolve-path (raw-path type-or-extensions &optional allow-examples)
  "Resolve RAW-PATH based on TYPE-OR-EXTENSIONS."
  (let* ((root (dragonruby--find-project-root))
         (current-dir (file-name-directory (or buffer-file-name default-directory))))
    (if (listp type-or-extensions)
        (let ((clean-path (string-trim (string-trim raw-path "['\"]") "['\"]")))
          (when (and root (not (string-empty-p clean-path)))
            (let* ((abs-path (expand-file-name clean-path root))
                   (ext (file-name-extension abs-path)))
              (when (and (file-exists-p abs-path)
                         (or (null type-or-extensions)
                             (and ext (member (downcase ext) type-or-extensions))))
                abs-path))))
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
         ((and full-path (file-exists-p full-path) (not (file-directory-p full-path)))
          full-path)
         ((and allow-examples root (not (string-prefix-p "examples/" raw-path)))
          (let ((ex-path (expand-file-name (concat "examples/" raw-path) root)))
            (if (and (file-exists-p ex-path) (not (file-directory-p ex-path))) ex-path nil)))
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
  "Check ImageMagick configuration."
  (interactive)
  (let ((cmd (or (executable-find "magick") (executable-find "convert"))))
    (if cmd (message "✅ SUCCESS: ImageMagick found at: %s" cmd)
      (message "❌ FAILURE: ImageMagick NOT found."))))

(defun dragonruby-run ()
  "Run the DragonRuby simulation in the current project root.
Launches as an Emacs subprocess to allow Stargate cable hookup."
  (interactive)
  (let* ((root (dragonruby--find-project-root))
         (binary (if (eq system-type 'windows-nt) "dragonruby.exe" "dragonruby"))
         (binary-path (and root (expand-file-name binary root))))
    (if (and binary-path (file-exists-p binary-path))
        (let ((default-directory root))
          (message "🐉 Stargate: Launching simulation from %s..." root)
          ;; Kill existing process if any to avoid port/file conflicts
          (when (get-process "dragonruby")
            (delete-process "dragonruby"))
          
          ;; Start as a dedicated Emacs process
          (let ((proc (start-process "dragonruby" "*DragonRuby Simulation*" binary-path)))
            ;; Re-install bridge via the standard installer for consistency
            (run-at-time 1 nil #'dragonruby-stargate-bridge-install proc)
            
            ;; Show the output buffer to the Architect
            (display-buffer "*DragonRuby Simulation*")
            (message "🚀 STARGATE: Simulation active. Cable connection pending...")))
      (if root
          (error "🐉 STARGATE: Binary 'dragonruby' not found in root: %s" root)
        (error "🐉 STARGATE: No project root found. Switch to your game code (main.rb) first!")))))

(defun dragonruby-set-project-root (dir)
  "Manually set the DragonRuby project root for the current buffer.
Use this if auto-detection fails or points to the wrong folder."
  (interactive "DSet DragonRuby Project Root: ")
  (let ((abs-path (expand-file-name dir)))
    (setq-local dragonruby--project-root-cache abs-path)
    (setq dragonruby--global-project-root abs-path)
    (message "🐲 Project Root manually set to: %s" abs-root)))

(defun dragonruby-diagnose ()
  "Show complete diagnostic information."
  (interactive)
  (let ((buf (get-buffer-create "*DragonRuby Diagnostics*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "🐲 DragonRuby Mode Diagnostics\n================================\n\n")
      (insert (format "Project Root: %s\n" (or (dragonruby--find-project-root) "NOT FOUND")))
      (insert (format "ImageMagick: %s\n" (if (or (executable-find "magick") (executable-find "convert")) "✅ FOUND" "❌ NOT FOUND")))
      (insert "\nRun M-x dragonruby-mode to toggle the mode.\n"))
    (display-buffer buf)))

(provide 'dragonruby-utils)
