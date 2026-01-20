;;; dragonruby-utils.el --- Generic utility functions -*- lexical-binding: t; -*-

;; Author: Macgyber <esteban3261g@gmail.com>
;; Version: 0.8.0
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/Macgyber/dragonruby-mode

;;; Commentary:
;; This module provides project detection, file collection, asset metadata
;; caching, and other generic utilities used by DragonRuby mode.

;;; Code:

(require 'cl-lib)


(defvar-local dragonruby--buffer-project-root :not-set
  "The anchored project root for this buffer.
Locking this value prevents recursive disk-crawling in idle pulses.")

(defvar dragonruby--project-ignore-list 
  '(".git" ".stargate" "node_modules" "$RECYCLE.BIN" "build" "native" "tmp")
  "Directories to strictly ignore during any manual scans.")

(defvar dragonruby--last-detected-project-root nil
  "The last valid project root detected globally.
Used during early boot or when no buffer-local root is anchored.")

(defun dragonruby--anchor-project-root ()
  "Locate and LOCK the project root for the current buffer.
This is an expensive procedure intended to run ONCE per buffer activation."
  (let* ((search-dir (and (buffer-file-name) (file-name-directory (buffer-file-name))))
         (root nil))
    (when (and search-dir (file-directory-p search-dir))
      (let* ((binary-root (locate-dominating-file search-dir
                             (lambda (dir)
                               (or (file-exists-p (expand-file-name "dragonruby" dir))
                                   (file-exists-p (expand-file-name "dragonruby.exe" dir))))))
             (content-root (locate-dominating-file search-dir "mygame")))
        (setq root content-root)
        
        ;; DOWN DRILL: If we don't find a parent with mygame/, look for it downwards
        (unless root
           (let ((sub-mygame (directory-files-recursively search-dir "^mygame$" t)))
             (when sub-mygame
               (setq root (file-name-directory (directory-file-name (file-name-directory (car sub-mygame))))))))))
    
    (setq dragonruby--buffer-project-root 
          (or (if root (expand-file-name root) nil)
              dragonruby--last-detected-project-root))
    
    (when dragonruby--buffer-project-root
      (setq dragonruby--last-detected-project-root dragonruby--buffer-project-root))
    
    dragonruby--buffer-project-root))

(defun dragonruby--find-project-root (&optional quiet)
  "Retrieve the anchored project root.
GUARDS: Never performs upward disk searches if called from a pulse."
  (if (eq dragonruby--buffer-project-root :not-set)
      (dragonruby--anchor-project-root)
    dragonruby--buffer-project-root))

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

;; --- ASSET METADATA ---
;; Caches expensive file attributes and image dimensions to stop CPU spikes.

(defvar dragonruby--asset-metadata-cache (make-hash-table :test 'equal)
  "Global cache for asset metadata. Key: absolute-path. Value: (attrs dims timestamp).")

(defvar dragonruby--asset-cache-ttl 60
  "Time-to-live for asset metadata in seconds (1 minute).")

(defun dragonruby--get-asset-metadata (path &optional force-dims)
  "Get metadata (attrs and optional dims) for asset at PATH.
Uses a centralized cache to avoid redundant create-image calls."
  (let* ((now (float-time))
         (cached (gethash path dragonruby--asset-metadata-cache)))
    (if (and cached (< (- now (nth 2 cached)) dragonruby--asset-cache-ttl))
        cached
      (let* ((attrs (file-attributes path))
             (dims (when (and force-dims attrs (> (file-attribute-size attrs) 0))
                     (let ((img (ignore-errors 
                                 (create-image path (dragonruby--get-image-type path) nil :nostrap t))))
                       (if img (image-size img t) '(0 . 0)))))
             (result (list attrs dims now)))
        (puthash path result dragonruby--asset-metadata-cache)
        result))))

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
         (current-dir (file-name-directory (or (buffer-file-name) default-directory))))
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
                        ((memq type '(require ruby sprite audio font data)) (or root current-dir))
                        (t (or root current-dir))))
             (ensure-rb (memq type '(require ruby require_relative)))
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
             (end (or (window-end nil t) (point-max)))
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
         ;; 1. Use detected root as primary
         (binary-path (and root (expand-file-name binary root))))
    
    ;; 2. If not in root, look upwards (Hierarchy Alignment)
    (unless (and binary-path (file-exists-p binary-path))
      (let ((parent-binary (locate-dominating-file (or root default-directory) binary)))
        (when parent-binary
          (setq binary-path (expand-file-name binary parent-binary)))))

    (if (and binary-path (file-exists-p binary-path))
        (let ((default-directory (file-name-directory binary-path)))
          (message "🐉 Stargate: Launching simulation from %s..." default-directory)
          ;; Kill existing process if any to avoid port/file conflicts
          (when (get-process "dragonruby")
            (delete-process "dragonruby"))
          
          ;; Start as a dedicated Emacs process
          (let ((proc (start-process "dragonruby" "*DragonRuby Simulation*" binary-path)))
            ;; Re-install bridge via the standard installer for consistency
            (when (fboundp 'dragonruby-stargate-bridge-find-and-install)
              (run-at-time 1 nil #'dragonruby-stargate-bridge-find-and-install proc))
            
            ;; Show the output buffer to the Architect
            (display-buffer "*DragonRuby Simulation*")
            (message "🚀 STARGATE: Simulation active. Cable connection pending...")))
      (if root
          (error "🐉 STARGATE: Binary 'dragonruby' not found in root or parents: %s" root)
        (error "🐉 STARGATE: No project root found. Switch to your game code (main.rb) first!")))))

(defun dragonruby-set-project-root (dir)
  "Manually set the DragonRuby project root for the current buffer.
Use this if auto-detection fails or points to the wrong folder."
  (interactive "DSet DragonRuby Project Root: ")
  (let ((abs-path (expand-file-name dir)))
    (setq-local dragonruby--buffer-project-root abs-path)
    (setq dragonruby--last-detected-project-root abs-path)
    (message "🐲 Project Root manually set to: %s" abs-path)))

(defun dragonruby-diagnose ()
  "Show complete diagnostic information, including module lifecycle status."
  (interactive)
  (let ((buf (get-buffer-create "*DragonRuby Diagnostics*"))
        (root (dragonruby--find-project-root))
        (modules (dragonruby-kernel-get-registered-modules)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert "🐲 DragonRuby Mode Diagnostics (v0.7.3)\n")
      (insert "========================================\n\n")
      
      (insert "📂 Project Sovereignty:\n")
      (insert (format "   Project Root: %s\n" (or root "NOT FOUND (Radar Blind)")))
      (insert (format "   Last Detected: %s\n" (or dragonruby--last-detected-project-root "NONE")))
      
      (insert "\n⚙️  External Assets:\n")
      (insert (format "   ImageMagick:  %s\n" (if (or (executable-find "magick") (executable-find "convert")) "✅ FOUND" "❌ NOT FOUND")))
      
      (insert "\n🧩 Lego Module Registry:\n")
      (insert "   NAME             TYPE      STATUS    REQUIRES\n")
      (insert "   ----             ----      ------    --------\n")
      (dolist (m modules)
        (let* ((name (plist-get m :name))
               (type (plist-get m :type))
               (status (dragonruby-module-status name))
               (reqs (plist-get m :requires)))
          (insert (format "   %-16s %-9s %-9s %s\n" 
                          name 
                          (or type "main")
                          (if (eq status :active) "✅ ACTIVE" "💤 INERT")
                          (or reqs "none")))))
      
      (insert "\n💡 Note: If a module is 'INERT' but enabled in .emacs, \n")
      (insert "   run M-x dragonruby-reset-and-reload to sync reality.\n")
      
      (insert "\n--- End of Ledger ---")
      (goto-char (point-min))
      (read-only-mode 1))
    (display-buffer buf)))

(provide 'dragonruby-utils)
;;; dragonruby-utils.el ends here
