;;; dragonruby-image-tools.el --- Image viewing and editing tools -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'image-file)
(require 'image-mode)
(require 'dragonruby-core)

(defcustom dragonruby-external-image-editor nil
  "Path to your preferred image editor.
If nil, uses system default.
Examples:
  \"C:/Program Files/GIMP/gimp.exe\"
  \"C:/Program Files/Aseprite/aseprite.exe\"
  \"/Applications/Aseprite.app/Contents/MacOS/aseprite\""
  :type '(choice (const :tag "System default" nil)
                 (file :tag "Custom editor path"))
  :group 'dragonruby)

;; --- IMAGE TOOLBAR UI ---

(defun dragonruby--goto-image ()
  "Move point to the first image in the buffer."
  (goto-char (point-min))
  (while (and (not (get-text-property (point) 'display))
              (not (eobp)))
    (forward-char 1)))

(defvar-local dragonruby--image-scale 1.0
  "Current scale factor for image zoom.")

(defun dragonruby-image-zoom-in ()
  "Zoom in the image."
  (interactive)
  (condition-case err
      (progn
        (setq dragonruby--image-scale (* dragonruby--image-scale 1.2))
        (image-transform-set-scale dragonruby--image-scale))
    (error (message "⚠️ Zoom not available for this image type"))))

(defun dragonruby-image-zoom-out ()
  "Zoom out the image."
  (interactive)
  (condition-case err
      (progn
        (setq dragonruby--image-scale (/ dragonruby--image-scale 1.2))
        (image-transform-set-scale dragonruby--image-scale))
    (error (message "⚠️ Zoom not available for this image type"))))

(defun dragonruby-image-rotate ()
  "Rotate the image 90 degrees."
  (interactive)
  (dragonruby--goto-image)
  (when (get-text-property (point) 'display)
    (image-rotate)))

(defun dragonruby--imagemagick-available-p ()
  "Check if ImageMagick is installed."
  (executable-find "magick"))

(defun dragonruby-image-trim ()
  "Trim transparent/white edges from image using ImageMagick."
  (interactive)
  (if (not (dragonruby--imagemagick-available-p))
      (message "❌ ImageMagick not found. Install from: https://imagemagick.org")
    (when buffer-file-name
      (let ((file buffer-file-name))
        (shell-command (format "magick \"%s\" -trim +repage \"%s\"" file file))
        (revert-buffer t t t)
        (message "✂️ Trimmed transparent/white edges")))))

(defun dragonruby-image-compress ()
  "Compress PNG image for better performance using ImageMagick."
  (interactive)
  (if (not (dragonruby--imagemagick-available-p))
      (message "❌ ImageMagick not found. Install from: https://imagemagick.org")
    (when buffer-file-name
      (let* ((file buffer-file-name)
             (size-before (file-attribute-size (file-attributes file))))
        (shell-command (format "magick \"%s\" -strip -quality 90 \"%s\"" file file))
        (let ((size-after (file-attribute-size (file-attributes file))))
          (revert-buffer t t t)
          (message "📦 Compressed: %.1f KB → %.1f KB (saved %.0f%%)"
                   (/ size-before 1024.0)
                   (/ size-after 1024.0)
                   (* 100 (- 1 (/ (float size-after) size-before)))))))))

(defun dragonruby-image-optimize ()
  "Show optimization options."
  (interactive)
  (if (not (dragonruby--imagemagick-available-p))
      (message "❌ ImageMagick not found. Install from: https://imagemagick.org")
    (message "Optimization: Use [ ✂️ Trim ] or [ 📦 Compress ] buttons")))

;; --- ADVANCED IMAGE EDITING (ImageMagick) ---

(defun dragonruby-image-resize-2x ()
  "Double the image size (2x scale)."
  (interactive)
  (if (not (dragonruby--imagemagick-available-p))
      (message "❌ ImageMagick not found. Install from: https://imagemagick.org")
    (when buffer-file-name
      (let ((file buffer-file-name))
        (shell-command (format "magick \"%s\" -resize 200%% \"%s\"" file file))
        (revert-buffer t t t)
        (message "🔍 Resized to 2x")))))

(defun dragonruby-image-resize-half ()
  "Halve the image size (0.5x scale)."
  (interactive)
  (if (not (dragonruby--imagemagick-available-p))
      (message "❌ ImageMagick not found. Install from: https://imagemagick.org")
    (when buffer-file-name
      (let ((file buffer-file-name))
        (shell-command (format "magick \"%s\" -resize 50%% \"%s\"" file file))
        (revert-buffer t t t)
        (message "🔍 Resized to 0.5x")))))

(defun dragonruby-image-flip-h ()
  "Flip image horizontally (mirror)."
  (interactive)
  (if (not (dragonruby--imagemagick-available-p))
      (message "❌ ImageMagick not found. Install from: https://imagemagick.org")
    (when buffer-file-name
      (let ((file buffer-file-name))
        (shell-command (format "magick \"%s\" -flop \"%s\"" file file))
        (revert-buffer t t t)
        (message "↔️ Flipped horizontally")))))

(defun dragonruby-image-flip-v ()
  "Flip image vertically."
  (interactive)
  (if (not (dragonruby--imagemagick-available-p))
      (message "❌ ImageMagick not found. Install from: https://imagemagick.org")
    (when buffer-file-name
      (let ((file buffer-file-name))
        (shell-command (format "magick \"%s\" -flip \"%s\"" file file))
        (revert-buffer t t t)
        (message "↕️ Flipped vertically")))))

(defun dragonruby-image-grayscale ()
  "Convert image to grayscale."
  (interactive)
  (if (not (dragonruby--imagemagick-available-p))
      (message "❌ ImageMagick not found. Install from: https://imagemagick.org")
    (when buffer-file-name
      (let ((file buffer-file-name))
        (shell-command (format "magick \"%s\" -colorspace Gray \"%s\"" file file))
        (revert-buffer t t t)
        (message "🎨 Converted to grayscale")))))

(defun dragonruby-image-invert ()
  "Invert image colors."
  (interactive)
  (if (not (dragonruby--imagemagick-available-p))
      (message "❌ ImageMagick not found. Install from: https://imagemagick.org")
    (when buffer-file-name
      (let ((file buffer-file-name))
        (shell-command (format "magick \"%s\" -negate \"%s\"" file file))
        (revert-buffer t t t)
        (message "🔄 Colors inverted")))))

(defun dragonruby-image-remove-white-bg ()
  "Remove white background (make transparent)."
  (interactive)
  (if (not (dragonruby--imagemagick-available-p))
      (message "❌ ImageMagick not found. Install from: https://imagemagick.org")
    (when buffer-file-name
      (let ((file buffer-file-name))
        (shell-command (format "magick \"%s\" -fuzz 10%% -transparent white \"%s\"" file file))
        (revert-buffer t t t)
        (message "✨ White background removed")))))

(defun dragonruby-image-reset-zoom ()
  "Reset image by reloading the file completely."
  (interactive)
  (let ((file buffer-file-name))
    (when file
      (kill-buffer)
      (find-file file)
      (message "Image reloaded"))))

(defvar-local dragonruby--show-image-info t
  "Whether to show image info in header-line.")

(defun dragonruby-image-info ()
  "Toggle image info display in header-line."
  (interactive)
  (setq dragonruby--show-image-info (not dragonruby--show-image-info))
  (dragonruby--setup-image-header-line)
  (message "Image info: %s" (if dragonruby--show-image-info "ON" "OFF")))

(defun dragonruby-image-open-external ()
  "Open image in external editor.
Uses `dragonruby-external-image-editor' if set, otherwise system default."
  (interactive)
  (when buffer-file-name
    (if dragonruby-external-image-editor
        ;; Use custom editor
        (progn
          (start-process "dr-editor" nil dragonruby-external-image-editor buffer-file-name)
          (message "Opened in: %s" (file-name-nondirectory dragonruby-external-image-editor)))
      ;; Use system default
      (cond
       ((eq system-type 'windows-nt)
        (w32-shell-execute "open" buffer-file-name))
       ((eq system-type 'darwin)
        (start-process "open" nil "open" buffer-file-name))
       (t
        (start-process "xdg-open" nil "xdg-open" buffer-file-name)))
      (message "Opened in system default editor"))))

(defun dragonruby--make-header-button (label action help)
  "Create a clickable button string for header-line."
  (propertize label
              'mouse-face 'highlight
              'help-echo help
              'local-map (let ((map (make-sparse-keymap)))
                          (define-key map [header-line mouse-1] action)
                          map)))

(defun dragonruby--get-image-info-string ()
  "Get image metadata as a formatted string for header-line."
  (when buffer-file-name
    (condition-case nil
        (let* ((attrs (file-attributes buffer-file-name))
               (size (file-attribute-size attrs))
               (size-str (cond
                          ((> size 1048576) (format "%.1fMB" (/ size 1048576.0)))
                          ((> size 1024) (format "%.1fKB" (/ size 1024.0)))
                          (t (format "%dB" size))))
               (ext (upcase (or (file-name-extension buffer-file-name) "?")))
               (img (create-image buffer-file-name))
               (img-size (image-size img t))
               (width (truncate (car img-size)))
               (height (truncate (cdr img-size))))
          (propertize (format "%s %dx%d %s" ext width height size-str)
                      'face '(:weight bold)))
      (error "")))))

(defun dragonruby--setup-image-header-line ()
  "Set up header-line with image editing buttons."
  (setq header-line-format
        (list
         (dragonruby--make-header-button "[+]" #'dragonruby-image-zoom-in "Zoom In")
         " "
         (dragonruby--make-header-button "[-]" #'dragonruby-image-zoom-out "Zoom Out")
         " "
         (dragonruby--make-header-button "[1:1]" #'dragonruby-image-reset-zoom "Reset")
         " "
         (dragonruby--make-header-button "[Rot]" #'dragonruby-image-rotate "Rotate")
         " "
         (dragonruby--make-header-button "[Info]" #'dragonruby-image-info "Info")
         " "
         (dragonruby--make-header-button "[Open]" #'dragonruby-image-open-external "External")
         " | "
         (propertize "[Trim]" 'face '(:foreground "orange") 
                     'mouse-face 'highlight 'help-echo "Trim edges"
                     'local-map (let ((map (make-sparse-keymap)))
                                 (define-key map [header-line mouse-1] #'dragonruby-image-trim)
                                 map))
         " "
         (propertize "[Zip]" 'face '(:foreground "green")
                     'mouse-face 'highlight 'help-echo "Compress"
                     'local-map (let ((map (make-sparse-keymap)))
                                 (define-key map [header-line mouse-1] #'dragonruby-image-compress)
                                 map))
         " "
         (propertize "[2x]" 'face '(:foreground "cyan")
                     'mouse-face 'highlight 'help-echo "Double"
                     'local-map (let ((map (make-sparse-keymap)))
                                 (define-key map [header-line mouse-1] #'dragonruby-image-resize-2x)
                                 map))
         " "
         (propertize "[.5]" 'face '(:foreground "cyan")
                     'mouse-face 'highlight 'help-echo "Half"
                     'local-map (let ((map (make-sparse-keymap)))
                                 (define-key map [header-line mouse-1] #'dragonruby-image-resize-half)
                                 map))
         " "
         (dragonruby--make-header-button "[<>]" #'dragonruby-image-flip-h "Flip H")
         " "
         (dragonruby--make-header-button "[/\\]" #'dragonruby-image-flip-v "Flip V")
         " "
         (propertize "[Gry]" 'face '(:foreground "gray")
                     'mouse-face 'highlight 'help-echo "Grayscale"
                     'local-map (let ((map (make-sparse-keymap)))
                                 (define-key map [header-line mouse-1] #'dragonruby-image-grayscale)
                                 map))
         " "
         (propertize "[Inv]" 'face '(:foreground "magenta")
                     'mouse-face 'highlight 'help-echo "Invert"
                     'local-map (let ((map (make-sparse-keymap)))
                                 (define-key map [header-line mouse-1] #'dragonruby-image-invert)
                                 map))
         " "
         (propertize "[NoBG]" 'face '(:foreground "pink")
                     'mouse-face 'highlight 'help-echo "Remove BG"
                     'local-map (let ((map (make-sparse-keymap)))
                                 (define-key map [header-line mouse-1] #'dragonruby-image-remove-white-bg)
                                 map))
         ;; Show image info if enabled (dynamically evaluated)
         '(:eval (if dragonruby--show-image-info
                     (concat "  |  " (or (dragonruby--get-image-info-string) ""))
                   "")))))

(defun dragonruby--image-mode-hook ()
  "Activate toolbar if inside a DragonRuby project."
  (when (and buffer-file-name (locate-dominating-file buffer-file-name "app/main.rb"))
    (dragonruby--setup-image-header-line)))

(define-minor-mode dragonruby-image-tools-mode
  "DragonRuby Image Tools."
  :global t
  :group 'dragonruby
  (if dragonruby-image-tools-mode
      (add-hook 'image-mode-hook #'dragonruby--image-mode-hook)
    (remove-hook 'image-mode-hook #'dragonruby--image-mode-hook)))

(provide 'dragonruby-image-tools)
