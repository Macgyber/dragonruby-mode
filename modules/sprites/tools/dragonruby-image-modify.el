;;; dragonruby-image-modify.el --- ImageMagick wrappers and Backups -*- lexical-binding: t; -*-

(require 'dragonruby-core)

(defun dragonruby--imagemagick-available-p ()
  "Check if ImageMagick is installed."
  (or (executable-find "magick")
      (executable-find "convert")))

(defun dragonruby--get-magick-download-url ()
  "Get the ImageMagick download URL specific to the current OS."
  (let ((base "https://imagemagick.org/script/download.php"))
    (cond
     ((eq system-type 'darwin) (concat base "#macosx"))
     ((eq system-type 'windows-nt) (concat base "#windows"))
     (t (concat base "#linux")))))

(defun dragonruby-image--check-imagemagick ()
  "Check for ImageMagick and show a guided menu if missing.
Detects OS to provide the correct download link."
  (if (dragonruby--imagemagick-available-p)
      t
    (let* ((os-name (cond ((eq system-type 'darwin) "macOS")
                          ((eq system-type 'windows-nt) "Windows")
                          (t "Linux")))
           (msg (format "This feature requires ImageMagick (detecting %s). Please download it." os-name))
           (choice (completing-read (concat "💡 " msg " ")
                                    '("🌐 Go to Download (Section) " "❌ Cancel")
                                    nil t)))
      (when (string-prefix-p "🌐" choice)
        (browse-url (dragonruby--get-magick-download-url)))
      nil)))

;; --- TIMELINE SYSTEM (UNDO/REDO) ---

(defvar-local dragonruby--image-history-index 0
  "Current position in the image history stack.")

(defvar-local dragonruby--image-history-total 0
  "Total number of snapshots saved in history.")

(defun dragonruby--get-history-dir ()
  "Get or create the history directory for image undo/redo.
Uses system temp directory to avoid polluting project folders."
  (dragonruby--cache-dir "image-history"))

(defun dragonruby--get-history-path (index)
  "Get the path for a specific history version INDEX.
Preserves the original file extension."
  (when buffer-file-name
    (let ((hash (md5 buffer-file-name))
          (ext (or (file-name-extension buffer-file-name) "png"))
          (dir (dragonruby--get-history-dir)))
      (expand-file-name (format "%s_%d.%s" hash index ext) dir))))

(defun dragonruby-image--save-snapshot ()
  "Save the current state of the image to the history timeline."
  (when buffer-file-name
    (let ((next-index (1+ dragonruby--image-history-index)))
      (copy-file buffer-file-name (dragonruby--get-history-path next-index) t)
      (setq dragonruby--image-history-index next-index)
      (setq dragonruby--image-history-total next-index)
      (message "🎞️ Timeline: Snapshot %d saved" next-index))))

(defun dragonruby-image-undo ()
  "Step backwards in the image timeline."
  (interactive)
  (if (<= dragonruby--image-history-index 0)
      (message "🎞️ Timeline: Already at the original version")
    (let ((target-path (dragonruby--get-history-path (1- dragonruby--image-history-index))))
      (when (and target-path (file-exists-p target-path))
        (setq dragonruby--image-history-index (1- dragonruby--image-history-index))
        (copy-file target-path buffer-file-name t)
        ;; Force instant refresh by clearing cache and reverting
        (clear-image-cache buffer-file-name)
        (revert-buffer t t t)
        (message "⏪ Timeline: Rolled back to version %d/%d" 
                 dragonruby--image-history-index dragonruby--image-history-total)))))

(defun dragonruby-image-redo ()
  "Step forwards in the image timeline."
  (interactive)
  (if (>= dragonruby--image-history-index dragonruby--image-history-total)
      (message "🎞️ Timeline: Already at the latest version")
    (let ((target-path (dragonruby--get-history-path (1+ dragonruby--image-history-index))))
      (when (and target-path (file-exists-p target-path))
        (setq dragonruby--image-history-index (1+ dragonruby--image-history-index))
        (copy-file target-path buffer-file-name t)
        ;; Force instant refresh
        (clear-image-cache buffer-file-name)
        (revert-buffer t t t)
        (message "⏩ Timeline: Forward to version %d/%d" 
                 dragonruby--image-history-index dragonruby--image-history-total)))))

;; Initialize history by saving version 0 if it doesn't exist
(defun dragonruby-image-init-history ()
  "Initialize history for the current buffer image."
  (when (and buffer-file-name (not (file-exists-p (dragonruby--get-history-path 0))))
    (copy-file buffer-file-name (dragonruby--get-history-path 0) t)
    (setq dragonruby--image-history-index 0)
    (setq dragonruby--image-history-total 0)))

;; --- MODIFICATION COMMANDS ---

(defun dragonruby-image-trim ()
  "Trim transparent/white edges from image using ImageMagick."
  (interactive)
  (when (dragonruby-image--check-imagemagick)
    (when buffer-file-name
      (let ((file buffer-file-name))
        (dragonruby-image--save-snapshot)
        (shell-command (format "magick \"%s\" -trim +repage \"%s\"" file file))
        (revert-buffer t t t)
        (message "✂️ Trimmed transparent/white edges")))))

(defun dragonruby-image-compress ()
  "Compress PNG image for better performance using ImageMagick."
  (interactive)
  (when (dragonruby-image--check-imagemagick)
    (when buffer-file-name
      (let* ((file buffer-file-name)
             (size-before (file-attribute-size (file-attributes file))))
        (dragonruby-image--save-snapshot)
        (shell-command (format "magick \"%s\" -strip -quality 90 \"%s\"" file file))
        (let ((size-after (file-attribute-size (file-attributes file))))
          (revert-buffer t t t)
          (message "📦 Compressed: %.1f KB → %.1f KB (saved %.0f%%)"
                   (/ size-before 1024.0)
                   (/ size-after 1024.0)
                   (* 100 (- 1 (/ (float size-after) size-before)))))))))

(defun dragonruby-image-resize-2x ()
  "Double the image size (2x scale)."
  (interactive)
  (when (dragonruby-image--check-imagemagick)
    (when buffer-file-name
      (let ((file buffer-file-name))
        (dragonruby-image--save-snapshot)
        ;; Use -filter point to keep pixel art crisp (nearest neighbor)
        (shell-command (format "magick \"%s\" -filter point -resize 200%% \"%s\"" file file))
        (revert-buffer t t t)
        (message "🔍 Resized to 2x (Crisp Pixel Art)")))))

(defun dragonruby-image-resize-half ()
  "Halve the image size (0.5x scale)."
  (interactive)
  (when (dragonruby-image--check-imagemagick)
    (when buffer-file-name
      (let ((file buffer-file-name))
        (dragonruby-image--save-snapshot)
        ;; Use -filter point for crisp downscaling too
        (shell-command (format "magick \"%s\" -filter point -resize 50%% \"%s\"" file file))
        (revert-buffer t t t)
        (message "🔍 Resized to 0.5x (Crisp Pixel Art)")))))

(defun dragonruby-image-flip-h ()
  "Flip image horizontally (mirror)."
  (interactive)
  (when (dragonruby-image--check-imagemagick)
    (when buffer-file-name
      (let ((file buffer-file-name))
        (dragonruby-image--save-snapshot)
        (shell-command (format "magick \"%s\" -flop \"%s\"" file file))
        (revert-buffer t t t)
        (message "↔️ Flipped horizontally")))))

(defun dragonruby-image-flip-v ()
  "Flip image vertically."
  (interactive)
  (when (dragonruby-image--check-imagemagick)
    (when buffer-file-name
      (let ((file buffer-file-name))
        (dragonruby-image--save-snapshot)
        (shell-command (format "magick \"%s\" -flip \"%s\"" file file))
        (revert-buffer t t t)
        (message "↕️ Flipped vertically")))))

(defun dragonruby-image-grayscale ()
  "Convert image to grayscale."
  (interactive)
  (when (dragonruby-image--check-imagemagick)
    (when buffer-file-name
      (let ((file buffer-file-name))
        (dragonruby-image--save-snapshot)
        (shell-command (format "magick \"%s\" -colorspace Gray \"%s\"" file file))
        (revert-buffer t t t)
        (message "🎨 Converted to grayscale")))))

(defun dragonruby-image-invert ()
  "Invert image colors."
  (interactive)
  (when (dragonruby-image--check-imagemagick)
    (when buffer-file-name
      (let ((file buffer-file-name))
        (dragonruby-image--save-snapshot)
        (shell-command (format "magick \"%s\" -negate \"%s\"" file file))
        (revert-buffer t t t)
        (message "🔄 Colors inverted")))))

(defun dragonruby-image-remove-white-bg ()
  "Remove white background (make transparent)."
  (interactive)
  (when (dragonruby-image--check-imagemagick)
    (when buffer-file-name
      (let ((file buffer-file-name))
        (dragonruby-image--save-snapshot)
        (shell-command (format "magick \"%s\" -fuzz 10%% -transparent white \"%s\"" file file))
        (revert-buffer t t t)
        (message "✨ White background removed")))))

(defun dragonruby-image-crop (width height x y)
  "Crop image to WIDTHxHEIGHT+X+Y."
  (interactive "nWidth: \nnHeight: \nnX: \nnY: ")
  (when (dragonruby-image--check-imagemagick)
    (when buffer-file-name
      (let ((file buffer-file-name))
        (dragonruby-image--save-snapshot)
        (shell-command (format "magick \"%s\" -crop %dx%d+%d+%d +repage \"%s\"" 
                               file width height x y file))
        (revert-buffer t t t)
        (message "✂️  Cropped to %dx%d at %d,%d" width height x y)))))

(defun dragonruby-image-tint (color)
  "Tint the image with COLOR."
  (interactive "sColor (name or hex): ")
  (when (dragonruby-image--check-imagemagick)
    (when buffer-file-name
      (let ((file buffer-file-name))
        (dragonruby-image--save-snapshot)
        (shell-command (format "magick \"%s\" -fill \"%s\" -colorize 50%% \"%s\"" 
                               file color file))
        (revert-buffer t t t)
        (message "🎨 Tinted with %s" color)))))

(defun dragonruby-image-hard-reset ()
  "Emergency recovery: restore the original file from the beginning of history."
  (interactive)
  (let ((original (dragonruby--get-history-path 0)))
    (if (and original (file-exists-p original))
        (when (yes-or-no-p "⚠️ Restore ORIGINAL version (Reset EVERYTHING)? ")
          (copy-file original buffer-file-name t)
          (setq dragonruby--image-history-index 0)
          (clear-image-cache buffer-file-name)
          (revert-buffer t t t)
          (message "🛡️ Emergency Restore: Back to version 0 (Original)"))
      (message "❌ No original version found in history."))))

(defun dragonruby-image-to-png ()
  "Convert current image to PNG."
  (interactive)
  (when (dragonruby-image--check-imagemagick)
    (when buffer-file-name
      (let* ((file buffer-file-name)
             (new-file (concat (file-name-sans-extension file) ".png")))
        (shell-command (format "magick \"%s\" \"%s\"" file new-file))
        (kill-buffer)
        (find-file new-file)
        (message "✨ Converted to PNG: %s" (file-name-nondirectory new-file))))))

(defun dragonruby-image-optimize ()
  "Show optimization options."
  (interactive)
  (when (dragonruby-image--check-imagemagick)
    (message "Optimization: Use [ ✂️ Trim ] or [ 📦 Compress ] buttons")))

(provide 'dragonruby-image-modify)
