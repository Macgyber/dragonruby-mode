;;; dragonruby-guide--impl.el --- Documentation linkage and lookup impl -*- lexical-binding: t; -*-

(require 'dragonruby-kernel)
(require 'dragonruby-core)
(require 'dragonruby-knowledge)
(require 'org)

(defvar dragonruby-guide-directory
  (let ((current-dir (file-name-directory (or load-file-name buffer-file-name))))
    ;; From modules/guide/ -> ../../knowledge/definitions
    (expand-file-name "../../knowledge/definitions" current-dir))
  "Directory containing local DragonRuby knowledge definitions.")

(defvar dragonruby-guide--concept-map nil
  "Hash table mapping search terms to guide filenames.")

(defvar-local dragonruby--guide-last-concept nil
  "Stores the last concept shown to avoid redundant updates.")

(defvar dragonruby-guide--sidebar-buffer nil
  "Buffer used for the guide sidebar, if active.")

(defun dragonruby-guide--build-concept-map ()
  "Build mapping from concept keywords to .org files."
  (let ((map (make-hash-table :test 'equal)))
    (when (file-directory-p dragonruby-guide-directory)
      (dragonruby-knowledge-clear)
      (dolist (file (directory-files dragonruby-guide-directory nil "\\.org$"))
        (unless (string-prefix-p "_" file)
          (let* ((concept-name (file-name-sans-extension file))
                 (full-path (expand-file-name file dragonruby-guide-directory)))
            (puthash concept-name file map)
            (with-temp-buffer
              (insert-file-contents full-path)
              (goto-char (point-min))
              (when (re-search-forward "^\\*+ .*\n" nil t)
                (let* ((start (point))
                       (end (if (re-search-forward "^\n\\|^\\*+" nil t)
                                (match-beginning 0)
                              (point-max)))
                       (desc (string-trim (buffer-substring-no-properties start end))))
                  (unless (string-empty-p desc)
                    (dragonruby-knowledge-put concept-name 
                                             `(:id ,concept-name 
                                               :desc ,desc 
                                               :file ,file))))))))))
    map))

;;;###autoload
(defun dragonruby-guide-open-concept (concept-name)
  "Open CONCEPT-NAME in its Org file with narrowing."
  (interactive "sConcept: ")
  (unless dragonruby-guide--concept-map
    (setq dragonruby-guide--concept-map (dragonruby-guide--build-concept-map)))
  
  (let ((org-file (gethash concept-name dragonruby-guide--concept-map)))
    (if org-file
        (let ((full-path (expand-file-name org-file dragonruby-guide-directory)))
          (if (file-exists-p full-path)
              (progn
                (find-file-other-window full-path)
                (dragonruby-guide--apply-hi-fi-styling)
                (widen)
                (goto-char (point-min))
                (when (re-search-forward (format "^\\*+ %s" (regexp-quote concept-name)) nil t)
                  (if (fboundp 'org-fold-show-subtree)
                      (org-fold-show-subtree)
                    (org-show-subtree))
                  (org-narrow-to-subtree)
                  (goto-char (point-min))
                  (message "📖 Focused on: %s (C-x n w to widen)" concept-name)))
            (message "⚠️ Org file not found: %s" full-path)))
      (message "ℹ️ No guidance for '%s'" concept-name))))

;;;###autoload
(defun dragonruby-guide-toggle-sidebar ()
  "Toggle the guidance sidebar."
  (interactive)
  (let* ((sidebar-buffer dragonruby-guide--sidebar-buffer)
         (sidebar-exists (and sidebar-buffer (buffer-live-p sidebar-buffer)))
         (sidebar-window (and sidebar-exists (get-buffer-window sidebar-buffer t))))
    (if (and sidebar-exists sidebar-window)
        (progn
          (delete-window sidebar-window)
          (setq dragonruby-guide--sidebar-buffer nil)
          (message "📖 Sidebar closed"))
      (dragonruby-guide-open-concept-sidebar "args"))))

;;;###autoload
(defun dragonruby-guide-open-glossary ()
  "Open the high-fidelity comprehensive glossary."
  (interactive)
  (let ((full-path (expand-file-name "glossary.org" dragonruby-guide-directory)))
    (if (file-exists-p full-path)
        (progn
          (find-file full-path)
          (dragonruby-guide--apply-hi-fi-styling))
      (message "⚠️ Glossary file not found: %s" full-path))))

;;;###autoload
(defalias 'dragonruby-glosario #'dragonruby-guide-open-glossary)

;;;###autoload
(defun dragonruby-guide-open-concept-sidebar (concept-name)
  "Open CONCEPT-NAME in a persistent side window."
  (interactive "sConcept: ")
  (unless dragonruby-guide--concept-map
    (setq dragonruby-guide--concept-map (dragonruby-guide--build-concept-map)))
  
  (let ((org-file (gethash concept-name dragonruby-guide--concept-map)))
    (if org-file
        (let ((full-path (expand-file-name org-file dragonruby-guide-directory)))
          (if (file-exists-p full-path)
              (let ((buf (find-file-noselect full-path)))
                (display-buffer-in-side-window
                 buf
                 `((side . ,(bound-and-true-p dragonruby-guide-sidebar-position))
                   (slot . 0)
                   (window-width . ,(bound-and-true-p dragonruby-guide-sidebar-width))
                   (window-parameters . ((no-delete-other-windows . t)
                                        (no-other-window . nil)))))
                (with-current-buffer buf
                  (widen)
                  (goto-char (point-min))
                  (dragonruby-guide--apply-hi-fi-styling)
                  (when (re-search-forward (format "^\\*+ %s" (regexp-quote concept-name)) nil t)
                    (if (fboundp 'org-fold-show-subtree)
                        (org-fold-show-subtree)
                      (org-show-subtree))
                    (org-narrow-to-subtree)
                    (goto-char (point-min))))
                (setq dragonruby-guide--sidebar-buffer buf)
                (message "📖 Sidebar: %s (C-c C-d to toggle)" concept-name))
            (message "⚠️ Org file not found: %s" full-path)))
      (message "ℹ️ No guidance for '%s'" concept-name))))

;;;###autoload
(defun dragonruby-guide-enable ()
  "Enable guidance services."
  (dragonruby-knowledge-clear)
  (setq dragonruby-guide--concept-map (dragonruby-guide--build-concept-map))
  (when (bound-and-true-p dragonruby-mode-map)
    (define-key dragonruby-mode-map (kbd "C-c C-d") #'dragonruby-guide-toggle-sidebar))
  (message "📖 Guide Module Enabled"))

;;;###autoload
(defun dragonruby-guide-disable ()
  "Disable guidance services."
  (when (bound-and-true-p dragonruby-mode-map)
    (define-key dragonruby-mode-map (kbd "C-c C-d") nil))
  (let ((buf dragonruby-guide--sidebar-buffer))
    (with-current-buffer (get-buffer-create " *temp*") ;; Safety check for buffer-live-p
      (when (and buf (buffer-live-p buf))
        (let ((win (get-buffer-window buf t)))
          (when win (delete-window win)))
        (kill-buffer buf))))
  (dragonruby-knowledge-clear)
  (setq dragonruby-guide--concept-map nil)
  (setq dragonruby-guide--sidebar-buffer nil)
  (message "📖 Guide Module Disabled"))

(defun dragonruby-guide--apply-hi-fi-styling ()
  "Apply Godot-docs inspired styling to the current buffer."
  (when (derived-mode-p 'org-mode)
    (setq-local line-spacing 0.2)
    (setq-local left-margin-width 2)
    (setq-local right-margin-width 2)
    (set-window-margins nil left-margin-width right-margin-width)
    (visual-line-mode 1)
    (when (bound-and-true-p display-line-numbers-mode)
      (display-line-numbers-mode -1))
    ;; If org-modern is available, use it
    (when (fboundp 'org-modern-mode)
      (org-modern-mode 1))))

(provide 'dragonruby-guide--impl)
;;; dragonruby-guide--impl.el ends here
