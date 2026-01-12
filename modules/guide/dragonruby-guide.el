;;; dragonruby-guide.el --- Documentation linkage and lookup -*- lexical-binding: t; -*-

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

(defcustom dragonruby-experimental-concepts-guide nil
  "Enable experimental concept guidance."
  :type 'boolean
  :group 'dragonruby)

(defcustom dragonruby-guide-sidebar-position 'right
  "Position of the guidance sidebar.
Can be 'left, 'right, 'top, or 'bottom."
  :type '(choice (const left) (const right) (const top) (const bottom))
  :group 'dragonruby)

(defcustom dragonruby-guide-sidebar-width 0.3
  "Width ratio of the sidebar (0.0 to 1.0).
Example: 0.3 means 30% of frame width."
  :type 'float
  :group 'dragonruby)

(defvar dragonruby-guide--sidebar-buffer nil
  "Buffer used for the guide sidebar, if active.")

(defvar dragonruby-guide-debug nil
  "Enable atomic debug messages for guide module.")

(defcustom dragonruby-guide-follow-mode t 
  "If non-nil, the sidebar automatically follows the concept under the cursor."
  :type 'boolean
  :group 'dragonruby)

(defvar-local dragonruby--guide-last-concept nil
  "Stores the last concept shown to avoid redundant updates.")

;; 🧱 LEGACY MINOR MODE REMOVED

;; -----------------------------------------------------------------------------
;; 🧬 Lifecycle
;; -----------------------------------------------------------------------------

(defun dragonruby-guide--build-concept-map ()
  "Build mapping from concept keywords to .org files.
Auto-discovers all .org files and syncs content to the Knowledge Cache."
  (let ((map (make-hash-table :test 'equal)))
    (when (file-directory-p dragonruby-guide-directory)
      (dragonruby-knowledge-clear)
      ;; 1. Collect Files
      (dolist (file (directory-files dragonruby-guide-directory nil "\\.org$"))
        (unless (string-prefix-p "_" file) ; Skip templates
          (let* ((concept-name (file-name-sans-extension file))
                 (full-path (expand-file-name file dragonruby-guide-directory)))
            (puthash concept-name file map)
            ;; 2. Sync to Knowledge Cache (Memory Loading)
            (with-temp-buffer
              (insert-file-contents full-path)
              (goto-char (point-min))
              ;; Extraction: Skip front-matter and first heading
              (when (re-search-forward "^\\*+ .*\n" nil t)
                (let* ((start (point))
                       ;; Find end of paragraph or next heading
                       (end (if (re-search-forward "^\n\\|^\\*+" nil t)
                                (match-beginning 0)
                              (point-max)))
                       (desc (string-trim (buffer-substring-no-properties start end))))
                  ;; 3. Put into Unified Brain (Only if we have content)
                  (unless (string-empty-p desc)
                    (dragonruby-knowledge-put concept-name 
                                             `(:id ,concept-name 
                                               :desc ,desc 
                                               :file ,file))))))))))
    map))

(defun dragonruby-guide-sidebar-open-p ()
  "Check if the guidance sidebar is currently visible."
  (and dragonruby-guide--sidebar-buffer
       (get-buffer-window dragonruby-guide--sidebar-buffer t)))

(defun dragonruby-guide-show-concept (id)
  "Main entry point to show a concept in the sidebar. Used by Follow Mode."
  (when (and dragonruby-guide-follow-mode
             (not (equal id dragonruby--guide-last-concept)))
    (setq dragonruby--guide-last-concept id)
    (dragonruby-guide-open-concept-sidebar id)))

(defun dragonruby-guide-open-concept (concept-name)
  "Open CONCEPT-NAME in its Org file with narrowing (Focused Learning).
Falls back to simple message if file not found."
  (interactive "sConcept: ")
  (unless dragonruby-guide--concept-map
    (setq dragonruby-guide--concept-map (dragonruby-guide--build-concept-map)))
  
  (let ((org-file (gethash concept-name dragonruby-guide--concept-map)))
    (if org-file
        (let ((full-path (expand-file-name org-file dragonruby-guide-directory)))
          (if (file-exists-p full-path)
              (progn
                ;; Open in a new window
                (find-file-other-window full-path)
                ;; Widen first (in case already narrowed)
                (widen)
                ;; Jump to the main heading
                (goto-char (point-min))
                (when (re-search-forward (format "^\\*+ %s" (regexp-quote concept-name)) nil t)
                  (if (fboundp 'org-fold-show-subtree)
                      (org-fold-show-subtree)
                    (org-show-subtree))
                  (org-narrow-to-subtree)
                  (goto-char (point-min))
                  (message "📖 Focused on: %s (C-x n w to widen)" concept-name)))
            (message "⚠️ Org file exists in map but not on disk: %s" full-path)))
      (message "ℹ️ No guidance for '%s'" concept-name))))

;; -----------------------------------------------------------------------------
;; 🪟 Side Window Support (Optional, experimental)
;; -----------------------------------------------------------------------------

(defun dragonruby-guide-open-concept-sidebar (concept-name)
  "Open CONCEPT-NAME in a persistent side window.
This is an alternative to the default Focused Learning approach.
The sidebar persists across buffer changes and cannot be closed with C-x 1."
  (interactive "sConcept: ")
  (unless dragonruby-guide--concept-map
    (setq dragonruby-guide--concept-map (dragonruby-guide--build-concept-map)))
  
  (let ((org-file (gethash concept-name dragonruby-guide--concept-map)))
    (if org-file
        (let ((full-path (expand-file-name org-file dragonruby-guide-directory)))
          (if (file-exists-p full-path)
              (let ((buf (find-file-noselect full-path)))
                ;; Display in side window
                (display-buffer-in-side-window
                 buf
                 `((side . ,dragonruby-guide-sidebar-position)
                   (slot . 0)
                   (window-width . ,dragonruby-guide-sidebar-width)
                   (window-parameters . ((no-delete-other-windows . t)
                                        (no-other-window . nil)))))
                ;; Narrow in the sidebar buffer
                (with-current-buffer buf
                  (widen)
                  (goto-char (point-min))
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

(defun dragonruby-guide-toggle-sidebar ()
  "Toggle the guidance sidebar.
If sidebar is open, close it. If closed, open it with the last concept or a default."
  (interactive)
  (let* ((sidebar-buffer dragonruby-guide--sidebar-buffer)
         (sidebar-exists (and sidebar-buffer (buffer-live-p sidebar-buffer)))
         (sidebar-window (and sidebar-exists (get-buffer-window sidebar-buffer t))))
    (if (and sidebar-exists sidebar-window)
        ;; Sidebar is open, close it
        (progn
          (condition-case nil
              (if (fboundp 'window-toggle-side-windows)
                  (delete-window sidebar-window)
                (delete-window sidebar-window))
            (error
             ;; Safe fallback: ignore window and kill buffer
             (ignore-errors (delete-window sidebar-window))))
          (setq dragonruby-guide--sidebar-buffer nil)
          (message "📖 Sidebar closed"))
      ;; Sidebar is closed, open with default concept
      (dragonruby-guide-open-concept-sidebar "args"))))

(defun dragonruby-guide-enable ()
  "Enable guidance services."
  ;; 1. Wipe and Rebuild Memory
  (dragonruby-knowledge-clear)
  (setq dragonruby-guide--concept-map (dragonruby-guide--build-concept-map))
  
  ;; 2. Keybindings
  (when (bound-and-true-p dragonruby-mode-map)
    (define-key dragonruby-mode-map (kbd "C-c C-d") #'dragonruby-guide-toggle-sidebar))
  
  (message "📖 Guide Module Enabled (Knowledge Sync: %d definitions)" 
           (length (dragonruby-knowledge-all-ids))))

(defun dragonruby-guide-disable ()
  "Disable guidance services and kill all guide buffers/windows."
  ;; 1. Remove keybinding
  (when (bound-and-true-p dragonruby-mode-map)
    (define-key dragonruby-mode-map (kbd "C-c C-d") nil))
  
  ;; 2. Kill Sidebar (Zombie Extermination)
  (let ((buf dragonruby-guide--sidebar-buffer))
    (when (and buf (buffer-live-p buf))
      (let ((win (get-buffer-window buf t)))
        (when win (delete-window win)))
      (kill-buffer buf)))
  
  ;; 3. Wipe Memory
  (dragonruby-knowledge-clear)
  (setq dragonruby-guide--concept-map nil)
  (setq dragonruby-guide--sidebar-buffer nil)
  (message "📖 Guide Module Disabled (Memory Wiped)"))

;;;###autoload
(defun dragonruby-guide ()
  "Main entry point for DragonRuby Guide. Toggles the sidebar."
  (interactive)
  (dragonruby-guide-toggle-sidebar))

;; -----------------------------------------------------------------------------
;; 📜 Manifest
;; -----------------------------------------------------------------------------

(dragonruby-register-module
 :name 'guide
 :type :main
 :namespace "dragonruby-guide-"
 :provides '(:guidance)
 :requires nil
 :entry-point 'dragonruby-guide
 :enable-fn #'dragonruby-guide-enable
 :disable-fn #'dragonruby-guide-disable)

(provide 'dragonruby-guide)
;;; dragonruby-guide.el ends here
