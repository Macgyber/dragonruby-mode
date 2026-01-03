;;; dragonruby-project.el --- Project detection and root finding -*- lexical-binding: t; -*-

(defvar-local dragonruby--project-root-cache nil
  "Cache for the project root to avoid redundant disk I/O.")

(defun dragonruby--find-project-root ()
  "Find the root of the DragonRuby project with caching.
Looks for app/main.rb, dragonruby executable, or .dragonruby/ folder."
  (or dragonruby--project-root-cache
      (let ((dir (file-name-directory (or buffer-file-name default-directory))))
        (setq dragonruby--project-root-cache
              (or (locate-dominating-file dir "app/main.rb")
                  (locate-dominating-file dir "dragonruby")
                  (locate-dominating-file dir ".dragonruby/")
                  ;; If we are inside 'app' or 'sprites' but no main.rb found yet,
                  ;; locate 'app' but ensure it's a directory
                  (let ((app-parent (locate-dominating-file dir "app")))
                    (when (and app-parent (file-directory-p (expand-file-name "app" app-parent)))
                      app-parent)))))))

;; Alias for backward compatibility
(defalias 'dragonruby--project-root 'dragonruby--find-project-root)

(provide 'dragonruby-project)
