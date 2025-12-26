;;; run-tests.el --- Comprehensive test suite for dragonruby-mode -*- lexical-binding: t; -*-

;; Add load paths
(add-to-list 'load-path (expand-file-name "src"))
(add-to-list 'load-path (expand-file-name "src/core"))
(add-to-list 'load-path (expand-file-name "src/concepts"))
(add-to-list 'load-path (expand-file-name "src/ui"))
(add-to-list 'load-path (expand-file-name "src/mode"))
(add-to-list 'load-path (expand-file-name "src/modules"))

(defvar test-results '())
(defvar tests-passed 0)
(defvar tests-failed 0)

(defun test-log (status test-name &optional details)
  "Log test result."
  (let ((result (list :status status :test test-name :details details)))
    (push result test-results)
    (if (eq status 'pass)
        (setq tests-passed (1+ tests-passed))
      (setq tests-failed (1+ tests-failed)))
    (message "%s %s%s"
             (if (eq status 'pass) "✓" "✗")
             test-name
             (if details (format " - %s" details) ""))))

(defun test-equal (name expected actual)
  "Test if EXPECTED equals ACTUAL."
  (if (equal expected actual)
      (test-log 'pass name)
    (test-log 'fail name (format "Expected: %s, Got: %s" expected actual))))

(defun test-true (name condition)
  "Test if CONDITION is true."
  (if condition
      (test-log 'pass name)
    (test-log 'fail name "Condition was nil")))

(defun test-not-nil (name value)
  "Test if VALUE is not nil."
  (if value
      (test-log 'pass name)
    (test-log 'fail name "Value was nil")))

;; ============================================================================
;; TEST SUITE
;; ============================================================================

(message "\n========================================")
(message "DragonRuby Mode - Test Suite")
(message "========================================\n")

;; TEST 1: Load main module
(message "TEST 1: Loading main module...")
(condition-case err
    (progn
      (require 'dragonruby)
      (test-log 'pass "Load dragonruby.el"))
  (error
   (test-log 'fail "Load dragonruby.el" (error-message-string err))))

;; TEST 2: Verify registry exists
(message "\nTEST 2: Checking registry...")
(test-true "Registry hash table exists" (hash-table-p dragonruby--concepts))

;; TEST 3: Test individual concept loading
(message "\nTEST 3: Testing concept registration...")

(defvar expected-concepts
  '("tick"
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

(dolist (concept-id expected-concepts)
  (let ((concept (dragonruby-get-concept concept-id)))
    (test-not-nil (format "Concept '%s' registered" concept-id) concept)
    
    ;; Check key fields exist
    (when concept
      (test-not-nil (format "  '%s' has definition" concept-id)
                    (dragonruby-concept-definition concept))
      (test-not-nil (format "  '%s' has mental-model" concept-id)
                    (dragonruby-concept-mental-model concept)))))

;; TEST 4: Test eldoc symbol detection
(message "\nTEST 4: Testing eldoc symbol detection...")
(with-temp-buffer
  (insert "args.state.player_x = 10")
  (goto-char 1)
  (forward-char 5) ; Position at 'args.state'
  
  (let ((symbol (dragonruby--symbol-at-point)))
    (test-equal "Detect 'args.state'" "args.state" symbol)))

(with-temp-buffer
  (insert "args.inputs.keyboard.key_down.space")
  (goto-char 1)
  (forward-char 12) ; Position at 'args.inputs.keyboard'
  
  (let ((symbol (dragonruby--symbol-at-point)))
    (test-equal "Detect 'args.inputs.keyboard'" "args.inputs.keyboard" symbol)))

;; TEST 5: Test color detection regex
(message "\nTEST 5: Testing color detection...")
(with-temp-buffer
  (insert "[255, 0, 0]")
  (goto-char (point-min))
  (let ((found (re-search-forward "\\[\\s-*\\([0-9]+\\)[, ]+\\([0-9]+\\)[, ]+\\([0-9]+\\)" nil t)))
    (test-true "Detect RGB array [255, 0, 0]" found)))

(with-temp-buffer
  (insert "[255, 128, 64, 200]")
  (goto-char (point-min))
  (let ((found (re-search-forward "\\[\\s-*\\([0-9]+\\)[, ]+\\([0-9]+\\)[, ]+\\([0-9]+\\)\\(?:[, ]+\\([0-9]+\\)\\)?" nil t)))
    (test-true "Detect RGBA array [255, 128, 64, 200]" found)))

;; TEST 6: Test sprite path detection
(message "\nTEST 6: Testing sprite path detection...")
(with-temp-buffer
  (insert "path: \"sprites/player.png\"")
  (goto-char (point-min))
  (let ((found (re-search-forward "[\"']\\([^\"]+\\.\\([a-zA-Z0-9]+\\)\\)[\"']" nil t)))
    (test-true "Detect sprite path in double quotes" found)
    (when found
      (test-equal "Extract path" "sprites/player.png" (match-string 1))
      (test-equal "Extract extension" "png" (match-string 2)))))

(with-temp-buffer
  (insert "sprite = 'sprites/enemy.jpg'")
  (goto-char (point-min))
  (let ((found (re-search-forward "[\"']\\([^\"]+\\.\\([a-zA-Z0-9]+\\)\\)[\"']" nil t)))
    (test-true "Detect sprite path in single quotes" found)))

;; TEST 7: Test concept field completeness
(message "\nTEST 7: Testing concept completeness...")
(let ((args-concept (dragonruby-get-concept "args")))
  (when args-concept
    (test-not-nil "args has definition" (dragonruby-concept-definition args-concept))
    (test-not-nil "args has definition-es" (dragonruby-concept-definition-es args-concept))
    (test-not-nil "args has intention" (dragonruby-concept-intention args-concept))
    (test-not-nil "args has mental-model" (dragonruby-concept-mental-model args-concept))
    (test-not-nil "args has mental-model-es" (dragonruby-concept-mental-model-es args-concept))))

(let ((color-concept (dragonruby-get-concept "color-array")))
  (when color-concept
    (test-not-nil "color-array has definition" (dragonruby-concept-definition color-concept))
    (test-not-nil "color-array has definition-es" (dragonruby-concept-definition-es color-concept))
    (test-not-nil "color-array has mental-model" (dragonruby-concept-mental-model color-concept))
    (test-not-nil "color-array has problems" (dragonruby-concept-problems color-concept))
    (test-not-nil "color-array has limits" (dragonruby-concept-limits color-concept))))

;; ============================================================================
;; RESULTS
;; ============================================================================

(message "\n========================================")
(message "TEST RESULTS")
(message "========================================")
(message "Passed: %d" tests-passed)
(message "Failed: %d" tests-failed)
(message "Total:  %d" (+ tests-passed tests-failed))

(if (= tests-failed 0)
    (message "\n🎉 ALL TESTS PASSED! 🎉")
  (message "\n⚠️  SOME TESTS FAILED ⚠️"))

(message "========================================\n")

;; Exit with proper code
(kill-emacs (if (= tests-failed 0) 0 1))
