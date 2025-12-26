;;; test-concepts.el --- Test that all concepts are registered -*- lexical-binding: t; -*-

;; Load the plugin
(add-to-list 'load-path (expand-file-name "src"))
(add-to-list 'load-path (expand-file-name "src/core"))
(add-to-list 'load-path (expand-file-name "src/concepts"))
(add-to-list 'load-path (expand-file-name "src/ui"))
(add-to-list 'load-path (expand-file-name "src/mode"))
(add-to-list 'load-path (expand-file-name "src/modules"))

(require 'dragonruby)

;; Test function
(defun test-all-concepts ()
  "Test that all expected concepts are registered."
  (let ((expected-concepts '("tick"
                             "args"
                             "args.state"
                             "args.inputs"
                             "args.outputs"
                             "args.inputs.keyboard"
                             "args.inputs.mouse"
                             "args.outputs.sprites"
                             "args.outputs.labels"
                             "args.outputs.solids"
                             "color-array"))
        (missing '())
        (found '()))
    
    (dolist (concept-id expected-concepts)
      (let ((concept (dragonruby-get-concept concept-id)))
        (if concept
            (progn
              (push concept-id found)
              (message "✓ %s: %s" concept-id (dragonruby-concept-name concept)))
          (push concept-id missing)
          (message "✗ MISSING: %s" concept-id))))
    
    (message "\n=== SUMMARY ===")
    (message "Found: %d/%d concepts" (length found) (length expected-concepts))
    (message "Missing: %s" (if missing (mapconcat 'identity missing ", ") "NONE"))
    
    (if (null missing)
        (message "\n🎉 ALL CONCEPTS REGISTERED SUCCESSFULLY!")
      (message "\n⚠️  SOME CONCEPTS ARE MISSING!"))))

;; Run the test
(test-all-concepts)
