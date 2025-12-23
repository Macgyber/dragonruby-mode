;;; test-concepts.el --- Minimal smoke tests for concept registry

(require 'ert)
(require 'dragonruby-core-registry)
(require 'dragonruby-concept-args)

(ert-deftest dragonruby-test-args-exists ()
  "Smoke test: Ensure the 'args' concept is registered and has correct fields."
  (let ((concept (dragonruby-get-concept "args")))
    ;; 1. Does it exist?
    (should concept)
    
    ;; 2. Is the ID correct?
    (should (string= (dragonruby-concept-id concept) "args"))
    
    ;; 3. Is the definition populated?
    (should (> (length (dragonruby-concept-definition concept)) 10))
    
    ;; 4. Is the mental model present? (Critical for our philosophy)
    (should (dragonruby-concept-mental-model concept))))

(ert-deftest dragonruby-test-registry-population ()
  "Ensure the registry is not empty."
  (should (> (hash-table-count dragonruby--concepts) 0)))

;; Run instructions:
;; M-x eval-buffer
;; M-x ert-run-tests-interactively
