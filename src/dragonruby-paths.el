;;; dragonruby-paths.el --- Universal Code & Data Navigation with Autocomplete -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'dragonruby-core)

(defvar-local dragonruby--path-overlays nil
  "List of path overlays in the current buffer.")
(defvar dragonruby-data-extensions '("json" "txt" "csv" "tsv" "xml" "yml" "yaml"))

;; --- PATH RESOLUTION ---
;; Note: dragonruby--find-project-root is now in dragonruby-core.el

(defun dragonruby--resolve-path (raw-path type)
  (let* ((root (dragonruby--find-project-root))
         (candidate (if (and (eq type 'ruby) (not (string-suffix-p ".rb" raw-path)))
                        (concat raw-path ".rb")
                      raw-path)))
    (expand-file-name candidate root)))

;; --- AUTOCOMPLETE (CAPF) for Requires ---
(defun dragonruby--get-all-ruby-files ()
  "Recursively find all .rb files."
  (let* ((root (dragonruby--find-project-root))
         (files (directory-files-recursively root "\\.rb$")))
    (mapcar (lambda (f) 
              ;; Remove extension for require '...' style
              (file-name-sans-extension (file-relative-name f root))) 
            files)))

(defun dragonruby-path-completion-at-point ()
  "CAPF backend for Ruby Requires."
  (let ((state (syntax-ppss)))
    (when (nth 3 state) ;; Inside string?
      (save-excursion
        (goto-char (nth 8 state)) ;; Go to start of string
        (backward-sexp 1) ;; Jump back to see if previous word is 'require'
        (when (looking-at-p "\\(?:require\\|require_relative\\|load\\)")
          (let* ((start (nth 8 state))
                 (end (point)))
             (list (1+ start) (nth 1 (syntax-ppss))
                   (completion-table-dynamic
                    (lambda (_) (dragonruby--get-all-ruby-files)))
                   :exclusive 'no)))))))

;; --- OVERLAYS ---
(defun dragonruby--clear-path-overlays ()
  (mapc #'delete-overlay dragonruby--path-overlays)
  (setq dragonruby--path-overlays nil))

(defun dragonruby--make-path-overlay (start end text real-path type valid)
  (let* ((is-error (and (eq type 'ruby) (not valid)))
         (face-props (if valid 
                         '(:foreground "#61AFEF" :underline t)
                       (if is-error '(:foreground "#E06C75" :underline (:style wave :color "#E06C75")) nil)))
         (help (if valid (format "Jump to: %s" (file-relative-name real-path))
                 (if is-error "❌ Ruby file not found" nil))))
    
    (when face-props
      (let ((ov (make-overlay start end)))
        (overlay-put ov 'face face-props)
        (overlay-put ov 'help-echo help)
        (overlay-put ov 'dragonruby-path t)
        (when valid
          (overlay-put ov 'keymap 
                       (let ((map (make-sparse-keymap)))
                         (define-key map [mouse-1] (lambda () (interactive) (find-file real-path)))
                         map))
          (overlay-put ov 'mouse-face 'highlight))
        (push ov dragonruby--path-overlays)))))

;; --- SCANNING ---
(defun dragonruby--scan-paths ()
  (dragonruby--clear-path-overlays)
  (save-excursion
    (goto-char (point-min))
    ;; 1. RUBY REQUIRES
    (while (re-search-forward "\\(?:require\\|require_relative\\|load\\)\\s-*[( ]\\s-*[\"']\\([^\"']+\\)[\"']" nil t)
      (let* ((raw-path (match-string 1))
             (start (match-beginning 1))
             (end (match-end 1))
             (abs-path (dragonruby--resolve-path raw-path 'ruby))
             (exists (file-exists-p abs-path)))
        (when (or exists (string-prefix-p "app/" raw-path) (string-match-p "/" raw-path))
          (dragonruby--make-path-overlay start end raw-path abs-path 'ruby exists))))

    ;; 2. DATA FILES
    (goto-char (point-min))
    (while (re-search-forward "\"\\([^\"\n]+\\)\"" nil t)
      (let* ((raw-path (match-string 1))
             (start (match-beginning 1))
             (end (match-end 1))
             (ext (file-name-extension raw-path)))
        (when (and ext (member (downcase ext) dragonruby-data-extensions))
          (let* ((abs-path (dragonruby--resolve-path raw-path 'data))
                 (exists (file-exists-p abs-path)))
            (when exists
              (dragonruby--make-path-overlay start end raw-path abs-path 'data t))))))))

(defun dragonruby--after-path-change (_beg _end _len)
  "Debounced path scanning after buffer change."
  (dragonruby--debounce #'dragonruby--scan-paths 0.3))

(defun dragonruby--refresh-paths ()
  "Refresh path overlays when buffer becomes visible."
  (when (and dragonruby-paths-mode
             (eq (current-buffer) (window-buffer)))
    (dragonruby--scan-paths)))

(defun dragonruby--setup-path-capf ()
  (add-hook 'completion-at-point-functions #'dragonruby-path-completion-at-point nil t))

(define-minor-mode dragonruby-paths-mode
  "Universal Navigation."
  :lighter " DR-Path"
  (if dragonruby-paths-mode
      (progn
        (add-hook 'after-change-functions #'dragonruby--after-path-change nil t)
        (add-hook 'window-configuration-change-hook #'dragonruby--refresh-paths nil t)
        (dragonruby--setup-path-capf)
        (dragonruby--scan-paths))
    (remove-hook 'after-change-functions #'dragonruby--after-path-change t)
    (remove-hook 'window-configuration-change-hook #'dragonruby--refresh-paths t)
    (remove-hook 'completion-at-point-functions #'dragonruby-path-completion-at-point t)
    (dragonruby--clear-path-overlays)))

(provide 'dragonruby-paths)
