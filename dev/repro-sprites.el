;; Repro script to check sprite finding logic

(let ((default-directory (expand-file-name ".." (file-name-directory load-file-name))))
  (add-to-list 'load-path (expand-file-name "src" default-directory))
  (add-to-list 'load-path (expand-file-name "src/core" default-directory))
  (add-to-list 'load-path (expand-file-name "src/sprites" default-directory))
  (add-to-list 'load-path (expand-file-name "src/paths" default-directory))
  (add-to-list 'load-path (expand-file-name "src/colors" default-directory))
  (add-to-list 'load-path (expand-file-name "src/image-tools" default-directory))
  (add-to-list 'load-path (expand-file-name "src/concepts" default-directory)))

(require 'dragonruby-project)
(require 'dragonruby-sprite-fs)

;; Force root to be the examples directory for testing consistency
;; We assume we are running from project root or dev/
(let* ((proj-root (expand-file-name "examples" (file-name-parent-directory (file-name-directory load-file-name))))
       (default-directory proj-root))
  
  (message "Testing with Root: %s" proj-root)
  (message "Default Directory: %s" default-directory)
  
  (if (file-exists-p (expand-file-name "app/main.rb" proj-root))
      (message "✅ app/main.rb found.")
    (message "❌ app/main.rb NOT found."))

  (let ((found (dragonruby-sprite-fs-find-all)))
    (message "Sprites Found: %S" found)))
