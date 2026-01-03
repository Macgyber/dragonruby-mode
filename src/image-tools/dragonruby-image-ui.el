;;; dragonruby-image-ui.el --- Header line and UI elements -*- lexical-binding: t; -*-

(require 'face-remap)
(require 'dragonruby-core)
(require 'dragonruby-image-modify)
(require 'dragonruby-image-view)

(declare-function w32-shell-execute "w32fns.c")

(defcustom dragonruby-external-editor-presets
  '((darwin . (("Aseprite" . "/Applications/Aseprite.app/Contents/MacOS/aseprite")
               ("Photoshop" . "/Applications/Adobe Photoshop 2024/Adobe Photoshop 2024.app/Contents/MacOS/Adobe Photoshop 2024")
               ("GIMP" . "/Applications/GIMP.app/Contents/MacOS/gimp")
               ("Krita" . "/Applications/Krita.app/Contents/MacOS/krita")))
    (windows-nt . (("Aseprite" . "C:/Program Files/Aseprite/aseprite.exe")
                   ("Photoshop" . "C:/Program Files/Adobe/Adobe Photoshop 2024/Photoshop.exe")
                   ("Paint.NET" . "C:/Program Files/paint.net/PaintDotNet.exe")))
    (gnu/linux . (("GIMP" . "/usr/bin/gimp")
                  ("Krita" . "/usr/bin/krita")
                  ("Inkscape" . "/usr/bin/inkscape"))))
  "Presets for external image editors categorized by OS."
  :type '(repeat (cons symbol (repeat (cons string string))))
  :group 'dragonruby)

(defun dragonruby--get-os-presets ()
  "Get the list of editor presets for the current OS."
  (cdr (assoc system-type dragonruby-external-editor-presets)))

(defun dragonruby-select-external-editor ()
  "Select an external editor from categorized presets.
Includes direct web links for downloading tools."
  (interactive)
  (if (active-minibuffer-window)
      (message "Minibuffer is already in use!")
    (let (choice path result-name)
      (condition-case nil
          (unwind-protect
                (let* ((presets (dragonruby--get-os-presets))
                       (current-name (when dragonruby-external-image-editor
                                       (file-name-nondirectory dragonruby-external-image-editor)))
                       (launch-opt (when current-name (list (format "🚀 Launch %s" current-name))))
                       (magick-url (dragonruby--get-magick-download-url))
                     (os-name (cond ((eq system-type 'darwin) "macOS")
                                    ((eq system-type 'windows-nt) "Windows")
                                    (t "Linux")))
                     (web-links `(("🌐 Download Aseprite (aseprite.org)" . "https://www.aseprite.org/download/")
                                  ("🌐 Download Krita (krita.org)" . "https://krita.org/en/download/")
                                  (,(format "🌐 Download ImageMagick for %s" os-name) . ,magick-url)))
                       (options (append '("❌ Cancel") 
                                        launch-opt
                                        (mapcar #'car presets) 
                                        (mapcar #'car web-links)
                                        '("📁 Custom Path..."))))
                  (message nil)
                  (setq choice (completing-read "🛠️ Artist Portal: " options nil t))
                  
                  (cond
                   ((string= choice "❌ Cancel")
                    (setq path nil))
                   ((and current-name (string= choice (format "🚀 Launch %s" current-name)))
                    (setq path dragonruby-external-image-editor
                          result-name current-name))
                   ((string-prefix-p "🌐" choice)
                  (browse-url (cdr (assoc choice web-links)))
                  (message "Opening download page...")
                  (setq path nil))
                 (t
                  (setq path (if (string= choice "📁 Custom Path...")
                                 (read-file-name "Enter editor executable path: ")
                               (cdr (assoc choice presets))))
                  (when (and path (not (string-empty-p path)))
                    (setq result-name choice)))))
            (message nil)
            (abort-recursive-edit) ; Force exit any lingering state
            (redisplay t))
        (quit (setq path nil)))

      (if (and path (not (string-empty-p path)))
          (progn
            (customize-save-variable 'dragonruby-external-image-editor path)
            (message "🚀 Editor locked: %s" result-name)
            path)
        (unless (string-prefix-p "🌐" (or choice ""))
          (message "Selection cancelled."))
        nil))))

(defcustom dragonruby-external-image-editor nil
  "Path to your preferred image editor executable."
  :type '(choice (const :tag "Not set" nil)
                 (file :tag "Custom editor path"))
  :group 'dragonruby)

(defvar-local dragonruby--show-image-info nil
  "Whether to show image info in header-line.")

(defvar-local dragonruby--info-face-cookie nil
  "Cookie for the info-mode background remapping.")

(defvar-local dragonruby--ui-group-view nil
  "Toggle for View group visibility.")

(defvar-local dragonruby--ui-group-modify nil
  "Toggle for Transform group visibility.")

(defvar-local dragonruby--ui-group-color nil
  "Toggle for Color group visibility.")

(defvar-local dragonruby--ui-group-tools nil
  "Toggle for Tools group visibility.")

(defun dragonruby--toggle-ui-group (group)
  "Toggle a UI GROUP visibility and refresh header-line.
Accordion logic: opening one group closes others to prevent overflow."
  (let* ((target-var (intern (concat "dragonruby--ui-group-" group)))
         (target-state (not (symbol-value target-var))))
    ;; Close all groups first
    (setq dragonruby--ui-group-view nil
          dragonruby--ui-group-modify nil
          dragonruby--ui-group-color nil
          dragonruby--ui-group-tools nil)
    ;; Set the target group to its new state
    (set (make-local-variable target-var) target-state)
    (force-mode-line-update)
    (redisplay t)))

(defun dragonruby--adaptive-tool (label compact icon action help &optional face)
  "Create a tool button that shrinks based on window width."
  (let* ((width (window-total-width))
         (final-label (cond
                       ((> width 110) label)
                       ((> width 70)  compact)
                       (t icon))))
    (dragonruby--make-header-button final-label action help face)))

(defun dragonruby--header-adaptive-label (full compact icon group-var)
  "Return a label based on window width.
FULL is the desktop text, COMPACT is for laptops, ICON is for tiny screens."
  (let ((width (window-total-width))
        (expanded (symbol-value group-var)))
    (cond
     ((> width 120) (format " %s %s %s " (if expanded "📂" "📁") full (if expanded "▼" "▶")))
     ((> width 80)  (format " %s %s %s " (if expanded "📂" "📁") compact (if expanded "▼" "▶")))
     (t             (format " %s " icon)))))

(defun dragonruby--make-header-button (label action help &optional face)
  "Create a clickable button string with high visibility.
Consistent dark background with neon labels."
  (let* ((btn-bg "#282C34")
         (default-fg "#FFFFFF")
         (label-fg (or (plist-get face :foreground) default-fg)))
    (propertize (concat "  " label "  ")
                'face (append `(:box (:line-width 5 :style released-button :color "#555555")
                                :background ,btn-bg :foreground ,label-fg
                                :weight bold
                                :height 1.15)
                              face)
                'mouse-face `(:box (:line-width 5 :style pressed-button :color ,label-fg)
                              :background ,label-fg :foreground ,btn-bg)
                'help-echo help
                'local-map (let ((map (make-sparse-keymap)))
                            (define-key map [header-line mouse-1] action)
                            (when (and help (string-match-p "Edit" help))
                              (define-key map [header-line mouse-3] 
                                (lambda (_e) (interactive "e") (run-at-time 0.05 nil #'dragonruby-select-external-editor))))
                            map))))

(defun dragonruby--get-image-info-string ()
  "Get image metadata as a formatted string for header-line with clear icons."
  (when buffer-file-name
    (condition-case nil
        (let* ((attrs (file-attributes buffer-file-name))
               (size (file-attribute-size attrs))
               (size-str (cond
                          ((> size 1048576) (format "%.1f MB" (/ size 1048576.0)))
                          ((> size 1024) (format "%.1f KB" (/ size 1024.0)))
                          (t (format "%d B" size))))
               (ext (upcase (or (file-name-extension buffer-file-name) "?")))
               (img (create-image buffer-file-name))
               (img-size (image-size img t))
               (width (truncate (car img-size)))
               (height (truncate (cdr img-size))))
          (propertize (format " 🖼️ %s  |  📏 %dx%d  |  📦 %s " ext width height size-str)
                      'face '(:weight bold)))
      (error ""))))

(defun dragonruby-image-info ()
  "Toggle image info display and 'Debug Stage' background."
  (interactive)
  (setq dragonruby--show-image-info (not dragonruby--show-image-info))
  ;; Toggle 'Debug Stage' (high-contrast background to see margins)
  (if dragonruby--show-image-info
      (setq dragonruby--info-face-cookie 
            (face-remap-add-relative 'default '(:background "#333333")))
    (when dragonruby--info-face-cookie
      (face-remap-remove-relative dragonruby--info-face-cookie)
      (setq dragonruby--info-face-cookie nil)))
  (dragonruby--setup-image-header-line)
  (message "🔍 Visual Debug Mode: %s (Reveal margins)" 
           (if dragonruby--show-image-info "ON" "OFF")))

(defun dragonruby-image-open-external ()
  "Open image (or its source file) in external editor.
If no editor is set, prompts the user to select one."
  (interactive)
  (when buffer-file-name
    (let* ((source (dragonruby--find-source-file buffer-file-name))
           (target-file (or source buffer-file-name))
           (editor (or dragonruby-external-image-editor
                       (dragonruby-select-external-editor))))
      
      (when editor
        (if source
            (message "🎨 Smart Jump: Opening source artwork [%s] in %s" 
                     (file-name-nondirectory source) (file-name-nondirectory editor))
          (message "✎ Edit: Opening [%s] in %s" 
                   (file-name-nondirectory target-file) (file-name-nondirectory editor)))
        
        (cond
         ((eq system-type 'windows-nt)
          (start-process "dr-editor" nil editor (replace-regexp-in-string "/" "\\\\" target-file)))
         (t
          (start-process "dr-editor" nil editor target-file)))))))

(defun dragonruby--setup-image-header-line ()
  "Set up a fluid, responsive header-line that adapts to window width."
  (setq header-line-format
        '(:eval
          (let* ((width (window-total-width))
                 (show-meta (> width 100)))
            (list
             ;; GROUP: VIEW
             " "
             (dragonruby--make-header-button 
              (dragonruby--header-adaptive-label "VIEW" "V" "👁️" 'dragonruby--ui-group-view)
              (lambda (_e) (interactive "e") (dragonruby--toggle-ui-group "view"))
              "View controls"
              (if dragonruby--ui-group-view
                  '(:foreground "#000000" :background "#FF8C00" :height 0.9)
                `(:foreground "#FF8C00" :background "#222222" :height ,(if (> width 80) 1.2 0.9))))
             (if dragonruby--ui-group-view
                 (list
                  (dragonruby--adaptive-tool "<" "<" "<" #'dragonruby-image-undo "Timeline Back (Undo)")
                  (if (> width 90) " " "")
                  (dragonruby--adaptive-tool "RST" "RST" "🧪" #'dragonruby-image-hard-reset "Hard Reset (Original version)" '(:foreground "#FF5555" :weight bold))
                  (if (> width 90) " " "")
                  (dragonruby--adaptive-tool ">" ">" ">" #'dragonruby-image-redo "Timeline Forward (Redo)")
                  (if (> width 90) " " "")
                  (dragonruby--adaptive-tool "+" "+" "+" #'dragonruby-image-zoom-in "Zoom In")
                  (if (> width 90) " " "")
                  (dragonruby--adaptive-tool "-" "-" "-" #'dragonruby-image-zoom-out "Out")
                  (if (> width 90) " " "")
                  (dragonruby--adaptive-tool "1:1" "1:1" "1" #'dragonruby-image-reset-zoom "Reset Zoom & Reload")
                  (if (> width 90) " " "")
                  (dragonruby--adaptive-tool "Rot" "R" "⟳" #'dragonruby-image-rotate "Rotate 90°"))
               "")

             ;; GROUP: TRANSFORM
             (if (> width 90) " " "")
             (dragonruby--make-header-button 
              (dragonruby--header-adaptive-label "TRANSFORM" "TR" "🧱" 'dragonruby--ui-group-modify)
              (lambda (_e) (interactive "e") (dragonruby--toggle-ui-group "modify"))
              "Transform controls" 
              (if dragonruby--ui-group-modify
                  '(:foreground "#000000" :background "#00CED1" :height 0.9)
                `(:foreground "#00CED1" :background "#222222" :height ,(if (> width 80) 1.2 0.9))))
             (if dragonruby--ui-group-modify
                 (list
                  (if (> width 90) "  " " ")
                  (dragonruby--adaptive-tool "Trim" "T" "✂" #'dragonruby-image-trim "Trim empty margins" '(:foreground "orange"))
                  (if (> width 90) " " "")
                  (dragonruby--adaptive-tool "Zip" "Z" "📦" #'dragonruby-image-compress "Compress & Strip Meta" '(:foreground "green"))
                  (if (> width 90) " " "")
                  (dragonruby--adaptive-tool "2x" "2x" "+" #'dragonruby-image-resize-2x "Scale 2x (Pixel Art)" '(:foreground "cyan"))
                  (if (> width 90) " " "")
                  (dragonruby--adaptive-tool ".5" ".5" "-" #'dragonruby-image-resize-half "Scale 0.5x" '(:foreground "cyan"))
                  (if (> width 90) " " "")
                  (dragonruby--adaptive-tool "<>" "<>" "↔" #'dragonruby-image-flip-h "Flip Horizontal")
                  (if (> width 90) " " "")
                  (dragonruby--adaptive-tool "/\\" "/\\" "↕" #'dragonruby-image-flip-v "Flip Vertical"))
               "")

             ;; GROUP: COLOR
             (if (> width 90) " " "")
             (dragonruby--make-header-button 
              (dragonruby--header-adaptive-label "COLOR" "C" "🎨" 'dragonruby--ui-group-color)
              (lambda (_e) (interactive "e") (dragonruby--toggle-ui-group "color"))
              "Color controls" 
              (if dragonruby--ui-group-color
                  '(:foreground "#000000" :background "#A1D95A" :height 0.9)
                `(:foreground "#A1D95A" :background "#222222" :height ,(if (> width 80) 1.2 0.9))))
             (if dragonruby--ui-group-color
                 (list
                  (if (> width 90) "  " " ")
                  (dragonruby--adaptive-tool "Gry" "G" "◑" #'dragonruby-image-grayscale "Convert to Grayscale" '(:foreground "gray"))
                  (if (> width 90) " " "")
                  (dragonruby--adaptive-tool "Inv" "I" "◐" #'dragonruby-image-invert "Invert Colors (Negative)" '(:foreground "magenta"))
                  (if (> width 90) " " "")
                  (dragonruby--adaptive-tool "NBG" "N" "✧" #'dragonruby-image-remove-white-bg "Remove Solid Background" '(:foreground "pink"))
                  (if (> width 90) " " "")
                  (dragonruby--adaptive-tool "Tint" "T" "☀" #'dragonruby-image-tint "Tint Image with Color" '(:foreground "yellow")))
               "")

             ;; GROUP: SYSTEM
             (if (> width 90) " " "")
             (dragonruby--make-header-button 
              (dragonruby--header-adaptive-label "SYSTEM" "S" "⚙️" 'dragonruby--ui-group-tools)
              (lambda (_e) (interactive "e") (dragonruby--toggle-ui-group "tools"))
              "System controls" 
              (if dragonruby--ui-group-tools
                  '(:foreground "#000000" :background "#FFFFFF" :height 0.9)
                `(:foreground "#FFFFFF" :background "#222222" :height ,(if (> width 80) 1.2 0.9))))
             (if dragonruby--ui-group-tools
                 (list
                  (if (> width 90) "  " " ")
                  (dragonruby--adaptive-tool "Info" "Inf" "ℹ" #'dragonruby-image-info "Info & Visual Debug")
                  (if (> width 90) " " "")
                  (dragonruby--adaptive-tool "Crop" "C" "⚃" #'dragonruby-image-crop "Numerical Crop (W H X Y)" '(:foreground "yellow"))
                  (if (> width 90) " " "")
                  (dragonruby--adaptive-tool "PNG" "P" "⎙" #'dragonruby-image-to-png "Export to PNG" '(:foreground "yellow"))
                  (if (> width 90) " " "")
                  (dragonruby--adaptive-tool "Edit" "E" "✎" #'dragonruby-image-open-external "Open in External Editor" '(:foreground "#55FF55")))
               "")

             ;; METADATA (Hidden on small screens)
             (if (and dragonruby--show-image-info show-meta)
                 (concat "  " (or (dragonruby--get-image-info-string) ""))
               ""))))))

;; Initialize history when image mode starts
(add-hook 'image-mode-hook #'dragonruby-image-init-history)

(provide 'dragonruby-image-ui)
