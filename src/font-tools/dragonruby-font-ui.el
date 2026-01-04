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
Currently supports \"DaFont\"."
  :type '(repeat string)
  :group 'dragonruby-fonts)

(defvar-local dragonruby--font-current-template 'full
  "Current template being displayed in the font viewer.")

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

(defun dragonruby-font-viewer-switch-template (template)
  "Switch the font viewer to a different TEMPLATE."
  (interactive)
  (setq dragonruby--font-current-template template)
  (dragonruby-font-viewer-refresh))

(defun dragonruby-font-viewer-refresh ()
  "Refresh the current font viewer buffer."
  (interactive)
  (when (and buffer-file-name (derived-mode-p 'dragonruby-font-viewer-mode))
    (let* ((root (dragonruby--find-project-root))
           (history-dir (expand-file-name ".dr_history/font-tools" root))
           (hash (md5 (format "%s-%s-%s" buffer-file-name (file-attribute-modification-time (file-attributes buffer-file-name)) dragonruby--font-current-template)))
           (preview-path (expand-file-name (concat hash ".png") history-dir)))
      
      (unless (file-directory-p history-dir) (make-directory history-dir t))
      (unless (file-exists-p preview-path)
        (dragonruby--render-font-sheet buffer-file-name preview-path dragonruby--font-current-template))
      
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-image (create-image preview-path 'png nil :scale 1.0))
        (set-buffer-modified-p nil)))))

(defun dragonruby--setup-font-header-line ()
  "Set up the header-line for the font viewer."
  (setq header-line-format
        '(:eval
          (let ((width (window-total-width)))
            (list
             " "
             (dragonruby--font-make-header-button 
              "SAMPLE" 
              (lambda (_e) (interactive "e") (dragonruby-font-viewer-switch-template 'full))
              "Show full font sample"
              (if (eq dragonruby--font-current-template 'full) '(:foreground "#55FF55") nil))
             " "
             (dragonruby--font-make-header-button 
              "ABC" 
              (lambda (_e) (interactive "e") (dragonruby-font-viewer-switch-template 'abc))
              "Show character set"
              (if (eq dragonruby--font-current-template 'abc) '(:foreground "#55FF55") nil))
             " "
             (dragonruby--font-make-header-button 
              "PANGRAM" 
              (lambda (_e) (interactive "e") (dragonruby-font-viewer-switch-template 'pangram))
              "Show pangram text"
              (if (eq dragonruby--font-current-template 'pangram) '(:foreground "#55FF55") nil))
             "  |  "
             (unless (member "DaFont" dragonruby-hidden-font-tools)
               (dragonruby--font-make-header-button 
                "DaFont" 
                (lambda (_e) (interactive "e") (browse-url "https://www.dafont.com/"))
                "DaFont - Resources" '(:foreground "#FF5555")))
             
             ;; CUSTOM FONT LINKS
             (mapconcat 
              (lambda (item)
                (let ((name (nth 0 item))
                      (target (nth 1 item))
                      (face (nth 2 item)))
                  (concat " "
                          (dragonruby--font-make-header-button 
                           name (lambda (_e) (interactive "e") (browse-url target))
                           (format "Open %s" target) face))))
              dragonruby-user-font-links "")

             "  |  "
             (dragonruby--font-make-header-button 
              "RELOAD" 
              (lambda (_e) (interactive "e") (dragonruby-font-viewer-refresh))
              "Refresh preview"
              '(:foreground "#00FFFF")))))))

(provide 'dragonruby-font-ui)
;;; dragonruby-font-ui.el ends here
