;;; simple-test.el --- Simple test to verify loading -*- lexical-binding: t; -*-

;; Add paths
(add-to-list 'load-path (expand-file-name "src"))
(add-to-list 'load-path (expand-file-name "src/core"))
(add-to-list 'load-path (expand-file-name "src/concepts"))
(add-to-list 'load-path (expand-file-name "src/ui"))
(add-to-list 'load-path (expand-file-name "src/mode"))
(add-to-list 'load-path (expand-file-name "src/modules"))

(defvar test-output "")

(defun test-append (msg)
  (setq test-output (concat test-output msg "\n")))

(test-append "========================================")
(test-append "DragonRuby Mode - Simple Test")
(test-append "========================================")
(test-append "")

;; Try to load
(condition-case err
    (progn
      (require 'dragonruby)
      (test-append "✓ Successfully loaded dragonruby.el"))
  (error
   (test-append (format "✗ Failed to load: %s" (error-message-string err)))))

(test-append "")
(test-append "Checking concepts:")
(test-append "")

(let ((expected '("tick" "args" "args.state" "args.inputs" "args.outputs"
                  "args.inputs.keyboard" "args.inputs.mouse"
                  "args.outputs.sprites" "args.outputs.labels" "args.outputs.solids"
                  "color-array"))
      (found 0)
      (missing 0))
  
  (dolist (id expected)
    (let ((concept (dragonruby-get-concept id)))
      (if concept
          (progn
            (test-append (format "  ✓ %-25s - %s" id (dragonruby-concept-name concept)))
            (setq found (1+ found)))
        (test-append (format "  ✗ %-25s - NOT FOUND" id))
        (setq missing (1+ missing)))))
  
  (test-append "")
  (test-append "========================================")
  (test-append (format "Results: %d found, %d missing" found missing))
  (test-append "========================================"))

;; Write to file
(with-temp-file "TEST_RESULTS.txt"
  (insert test-output))

;; Also print
(princ test-output)
