;;; dragonruby-font-ui.el --- Header line and UI for font viewer -*- lexical-binding: t; -*-

(require 'dragonruby-core)
(require 'dragonruby-font-engine)

(defgroup dragonruby-fonts nil
  "Settings for DragonRuby font management."
  :group 'dragonruby)

(defcustom dragonruby-user-font-links nil
  "Custom tools for the Font Creative Hub.
Each item is a list: (Name URL/Path Icon-Face).
Example: '(\"Google Fonts\" \"https://fonts.google.com/\" \"(:foreground \\\"#4285F4\\\")\")"
  :type '(repeat (list string string sexp))
  :group 'dragonruby-fonts)

(defcustom dragonruby-hidden-font-tools nil
  "List of predefined Font Creative Hub tools to hide.
Currently supports \"DaFont\", \"FontSquirrel\"."
  :type '(repeat string)
  :group 'dragonruby-fonts)

;; --- STATE VARIABLES ---

(defvar-local dragonruby--font-current-template 'full
  "Current template being displayed in the font viewer.")

(defvar-local dragonruby--font-current-scale 1.0
  "Current scale factor for font preview (1.0 = 100%).")

(defvar-local dragonruby--font-custom-text nil
  "Custom text entered by user for preview.")

;; --- ACCORDION GROUP TOGGLES ---

(defvar-local dragonruby--font-group-templates nil
  "Toggle for Templates group visibility.")

(defvar-local dragonruby--font-group-view nil
  "Toggle for View group visibility.")

(defvar-local dragonruby--font-group-creative nil
  "Toggle for Creative Hub group visibility.")

(defun dragonruby--font-toggle-group (group)
  "Toggle a font UI GROUP visibility with accordion logic."
  (let* ((target-var (intern (concat "dragonruby--font-group-" group)))
         (target-state (not (symbol-value target-var))))
    ;; Close all groups first (accordion behavior)
    (setq dragonruby--font-group-templates nil
          dragonruby--font-group-view nil
          dragonruby--font-group-creative nil)
    ;; Set target group to new state
    (set (make-local-variable target-var) target-state)
    (force-mode-line-update)
    (redisplay t)))

;; --- UI HELPERS ---

(defun dragonruby--font-make-header-button (label action help &optional face)
  "Create a clickable button for the font header-line."
  (let* ((btn-bg "#282C34")
         (default-fg "#FFFFFF")
         (label-fg (or (plist-get face :foreground) default-fg)))
    (propertize (concat "  " label "  ")
                'face (append `(:box (:line-width 5 :style released-button :color "#555555")
                                :background ,btn-bg :foreground ,label-fg
                                :weight bold
                                :height 1.1)
                               face)
                'mouse-face `(:box (:line-width 5 :style pressed-button :color ,label-fg)
                               :background ,label-fg :foreground ,btn-bg)
                'help-echo help
                'local-map (let ((map (make-sparse-keymap)))
                            (define-key map [header-line mouse-1] action)
                            map))))

(defun dragonruby--font-adaptive-tool (label compact icon action help &optional face)
  "Create a tool button that shrinks based on window width."
  (let* ((width (window-total-width))
         (final-label (cond
                       ((> width 110) label)
                       ((> width 70)  compact)
                       (t icon))))
    (dragonruby--font-make-header-button final-label action help face)))

(defun dragonruby--font-header-adaptive-label (full compact icon group-var)
  "Return a label based on window width with expand/collapse indicators."
  (let ((width (window-total-width))
        (expanded (symbol-value group-var)))
    (cond
     ((> width 120) (format " %s %s %s " (if expanded "📂" "📁") full (if expanded "▼" "▶")))
     ((> width 80)  (format " %s %s %s " (if expanded "📂" "📁") compact (if expanded "▼" "▶")))
     (t             (format " %s " icon)))))

;; --- FONT ACTIONS ---

(defun dragonruby-font-viewer-switch-template (template)
  "Switch the font viewer to a different TEMPLATE."
  (interactive)
  (setq dragonruby--font-current-template template)
  (dragonruby-font-viewer-refresh))

(defun dragonruby-font-preview-custom-text ()
  "Ask user for custom text and render preview."
  (interactive)
  (let ((text (read-string "Texto a previsualizar: " 
                           (or dragonruby--font-custom-text "Mi Juego DragonRuby"))))
    (when (not (string-empty-p text))
      (setq dragonruby--font-custom-text text)
      (setq dragonruby--font-current-template text)
      ;; Also update the inline preview text (for font overlays in code)
      (setq dragonruby-font-preview-text text)
      (dragonruby-font-viewer-refresh)
      (message "🎮 Previewing: \"%s\" (inline preview updated)" text))))

(defun dragonruby-font-zoom-in ()
  "Zoom in the font preview (increase scale)."
  (interactive)
  (setq dragonruby--font-current-scale (min 4.0 (+ dragonruby--font-current-scale 0.25)))
  (dragonruby-font-viewer-refresh)
  (message "🔍 Font Scale: %.0f%%" (* dragonruby--font-current-scale 100)))

(defun dragonruby-font-zoom-out ()
  "Zoom out the font preview (decrease scale)."
  (interactive)
  (setq dragonruby--font-current-scale (max 0.25 (- dragonruby--font-current-scale 0.25)))
  (dragonruby-font-viewer-refresh)
  (message "🔍 Font Scale: %.0f%%" (* dragonruby--font-current-scale 100)))

(defun dragonruby-font-reset-zoom ()
  "Reset font preview to 100% scale."
  (interactive)
  (setq dragonruby--font-current-scale 1.0)
  (dragonruby-font-viewer-refresh)
  (message "🔍 Font Scale: Reset to 100%%"))

(defun dragonruby-font-viewer-refresh ()
  "Refresh the current font viewer buffer."
  (interactive)
  (when (and buffer-file-name (derived-mode-p 'dragonruby-font-viewer-mode))
    (let* ((history-dir (dragonruby--cache-dir "font-tools"))
           (hash (md5 (format "%s-%s-%s" buffer-file-name (file-attribute-modification-time (file-attributes buffer-file-name)) dragonruby--font-current-template)))
           (preview-path (expand-file-name (concat hash ".png") history-dir)))
      
      (unless (file-exists-p preview-path)
        (dragonruby--render-font-sheet buffer-file-name preview-path dragonruby--font-current-template))
      
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-image (create-image preview-path 'png nil :scale dragonruby--font-current-scale))
        (set-buffer-modified-p nil)))))

;; --- MAIN HEADER LINE ---

(defun dragonruby--setup-font-header-line ()
  "Set up the header-line for the font viewer with accordion groups."
  (setq header-line-format
        '(:eval
          (let ((width (window-total-width)))
            (list
             " "
             ;; GROUP: VIEW (First - most common actions)
             (dragonruby--font-make-header-button 
              (dragonruby--font-header-adaptive-label "VIEW" "V" "�️" 'dragonruby--font-group-view)
              (lambda (_e) (interactive "e") (dragonruby--font-toggle-group "view"))
              "View controls (zoom, info)"
              (if dragonruby--font-group-view
                  '(:foreground "#000000" :background "#00CED1" :height 0.9)
                `(:foreground "#00CED1" :background "#222222" :height ,(if (> width 80) 1.2 0.9))))
             (if dragonruby--font-group-view
                 (list
                  (dragonruby--font-adaptive-tool "-" "-" "-" 
                   (lambda (_e) (interactive "e") (dragonruby-font-zoom-out))
                   "Zoom Out" nil)
                  (if (> width 90) " " "")
                  (dragonruby--font-adaptive-tool "+" "+" "+" 
                   (lambda (_e) (interactive "e") (dragonruby-font-zoom-in))
                   "Zoom In" nil)
                  (if (> width 90) " " "")
                  (dragonruby--font-adaptive-tool "1:1" "1:1" "1" 
                   (lambda (_e) (interactive "e") (dragonruby-font-reset-zoom))
                   "Reset to 100%" '(:foreground "#FF8C00"))
                  (if (> width 90) " " "")
                  (dragonruby--font-adaptive-tool "INFO" "I" "ℹ" 
                   (lambda (_e) (interactive "e") 
                     (message "📁 %s | 📏 %.0f%%" 
                              (file-name-nondirectory buffer-file-name)
                              (* dragonruby--font-current-scale 100)))
                   "Show font info" '(:foreground "#FFEB3B"))
                  (if (> width 90) " " "")
                  (dragonruby--font-adaptive-tool "RELOAD" "R" "�" 
                   (lambda (_e) (interactive "e") (dragonruby-font-viewer-refresh))
                   "Refresh preview" '(:foreground "#00FFFF")))
               "")

             ;; GROUP: TEMPLATES (Second)
             (if (> width 90) " " "")
             (dragonruby--font-make-header-button 
              (dragonruby--font-header-adaptive-label "TEMPLATES" "T" "�" 'dragonruby--font-group-templates)
              (lambda (_e) (interactive "e") (dragonruby--font-toggle-group "templates"))
              "Font sample templates"
              (if dragonruby--font-group-templates
                  '(:foreground "#000000" :background "#FF8C00" :height 0.9)
                `(:foreground "#FF8C00" :background "#222222" :height ,(if (> width 80) 1.2 0.9))))
             (if dragonruby--font-group-templates
                 (list
                  (dragonruby--font-adaptive-tool "SAMPLE" "S" "📋" 
                   (lambda (_e) (interactive "e") (dragonruby-font-viewer-switch-template 'full))
                   "Full font sample"
                   (if (eq dragonruby--font-current-template 'full) '(:foreground "#55FF55") nil))
                  (if (> width 90) " " "")
                  (dragonruby--font-adaptive-tool "ABC" "A" "🔤" 
                   (lambda (_e) (interactive "e") (dragonruby-font-viewer-switch-template 'abc))
                   "Character set"
                   (if (eq dragonruby--font-current-template 'abc) '(:foreground "#55FF55") nil))
                  (if (> width 90) " " "")
                  (dragonruby--font-adaptive-tool "PANGRAM" "P" "📖" 
                   (lambda (_e) (interactive "e") (dragonruby-font-viewer-switch-template 'pangram))
                   "Pangram text"
                   (if (eq dragonruby--font-current-template 'pangram) '(:foreground "#55FF55") nil))
                  (if (> width 90) " " "")
                  ;; TEST button for custom text preview
                  (dragonruby--font-adaptive-tool "TEST" "T" "🎮" 
                   (lambda (_e) (interactive "e") (dragonruby-font-preview-custom-text))
                   "Preview YOUR text (test game text)"
                   '(:foreground "#FF69B4")))
               "")

             ;; GROUP: CREATIVE (Font Resources Hub)
             (if (> width 90) " " "")
             (dragonruby--font-make-header-button 
              (dragonruby--font-header-adaptive-label "CREATIVE" "C" "�" 'dragonruby--font-group-creative)
              (lambda (_e) (interactive "e") (dragonruby--font-toggle-group "creative"))
              "Creative Hub | Font Resources"
              (if dragonruby--font-group-creative
                  '(:foreground "#000000" :background "#55FF55" :height 0.9)
                `(:foreground "#55FF55" :background "#222222" :height ,(if (> width 80) 1.2 0.9))))
             (if dragonruby--font-group-creative
                 (list
                  (unless (member "DaFont" dragonruby-hidden-font-tools)
                    (dragonruby--font-adaptive-tool "DaFont" "DF" "📂" 
                     (lambda (_e) (interactive "e") (browse-url "https://www.dafont.com/"))
                     "DaFont - Free Fonts" '(:foreground "#FF5555")))
                  (if (> width 90) " " "")
                  (unless (member "FontSquirrel" dragonruby-hidden-font-tools)
                    (dragonruby--font-adaptive-tool "FontSquirrel" "FS" "🐿️" 
                     (lambda (_e) (interactive "e") (browse-url "https://www.fontsquirrel.com/"))
                     "Font Squirrel - Free Fonts" '(:foreground "#FFB347")))
                  (if (> width 90) " " "")
                  (unless (member "GoogleFonts" dragonruby-hidden-font-tools)
                    (dragonruby--font-adaptive-tool "Google" "G" "🔤" 
                     (lambda (_e) (interactive "e") (browse-url "https://fonts.google.com/"))
                     "Google Fonts" '(:foreground "#4285F4")))
                  (if (> width 90) " " "")
                  (unless (member "GitHub" dragonruby-hidden-font-tools)
                    (dragonruby--font-adaptive-tool "GitHub" "GH" "🐙" 
                     (lambda (_e) (interactive "e") (browse-url "https://github.com/topics/fonts"))
                     "GitHub - Open Source Fonts" '(:foreground "#FFFFFF")))
                  ;; Custom user links
                  (mapconcat 
                   (lambda (item)
                     (let ((name (nth 0 item))
                           (target (nth 1 item))
                           (face (nth 2 item)))
                       (concat (if (> width 90) " " "")
                               (dragonruby--font-adaptive-tool 
                                name (substring name 0 (min 2 (length name))) "⭐"
                                `(lambda (_e) (interactive "e") (browse-url ,target))
                                (format "Open %s" target) face))))
                   dragonruby-user-font-links "")
                  (if (> width 90) " " "")
                  ;; ADD BUTTON
                  (dragonruby--font-adaptive-tool "+" "+" "➕" 
                   (lambda (_e) (interactive "e")
                     (if (active-minibuffer-window)
                         (message "Close minibuffer first (C-g)")
                       (let* ((name (read-string "Link name: "))
                              (url (read-string "URL: "))
                              (random-color (format "#%02X%02X%02X" 
                                                    (+ 100 (random 156))
                                                    (+ 100 (random 156))
                                                    (+ 100 (random 156))))
                              (color (read-string (format "Color [%s]: " random-color) nil nil random-color)))
                         (when (and (not (string-empty-p name)) 
                                    (not (string-empty-p url)))
                           (add-to-list 'dragonruby-user-font-links (list name url `(:foreground ,color)) t)
                           (customize-save-variable 'dragonruby-user-font-links dragonruby-user-font-links)
                           (force-mode-line-update)
                           (message "✅ Added: %s" name)))))
                   "Add Custom Link" '(:foreground "#00FF00"))
                  (if (> width 90) " " "")
                  ;; REMOVE BUTTON
                  (dragonruby--font-adaptive-tool "-" "-" "➖" 
                   (lambda (_e) (interactive "e")
                     (if (active-minibuffer-window)
                         (message "Close minibuffer first (C-g)")
                       (let* ((predefined '("DaFont" "FontSquirrel" "GoogleFonts"))
                              (visible-predefined (cl-remove-if (lambda (x) (member x dragonruby-hidden-font-tools)) predefined))
                              (user-names (mapcar #'car dragonruby-user-font-links))
                              (all-names (append visible-predefined user-names)))
                         (if (null all-names)
                             (message "No links to remove")
                           (let ((choice (completing-read "Hide/Remove: " all-names nil t)))
                             (if (member choice predefined)
                                 (progn
                                   (add-to-list 'dragonruby-hidden-font-tools choice)
                                   (customize-save-variable 'dragonruby-hidden-font-tools dragonruby-hidden-font-tools)
                                   (force-mode-line-update)
                                   (message "👁️ Hidden: %s" choice))
                               (setq dragonruby-user-font-links
                                     (cl-remove-if (lambda (x) (string= (car x) choice))
                                                   dragonruby-user-font-links))
                               (customize-save-variable 'dragonruby-user-font-links dragonruby-user-font-links)
                               (force-mode-line-update)
                               (message "🗑️ Removed: %s" choice)))))))
                   "Hide/Remove Link" '(:foreground "#FF5555")))
               ""))))))

(provide 'dragonruby-font-ui)
;;; dragonruby-font-ui.el ends here
