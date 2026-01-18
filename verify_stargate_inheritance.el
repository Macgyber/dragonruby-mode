
(require 'cl-lib)
(require 'json)

;; Setup load path
(add-to-list 'load-path default-directory)
(add-to-list 'load-path (expand-file-name "modules/core" default-directory))
(add-to-list 'load-path (expand-file-name "modules/stargate/emacs" default-directory))
(add-to-list 'load-path (expand-file-name "modules/stargate/sessions" default-directory))

(require 'dragonruby-stargate-manager)
(setq debug-on-error t)

;; Dummy project root function for batch testing
(unless (fboundp 'dragonruby--find-project-root)
  (defun dragonruby--find-project-root (&optional _) default-directory))

(defun test-stargate-inheritance ()
  (message "🧪 Testing Stargate Inheritance...")
  
  ;; 1. Init Session
  (dragonruby-stargate-session-init default-directory)
  
  ;; 2. Fill Prime (Ticks 0-9)
  ;; IMPORTANT: Keys must be STRINGS to match JSON-based manager logic
  (dotimes (i 10)
    (dragonruby-stargate-session-record-moment 
     `(("address" . ,(format "prime@%d" i))
       ("hash" . ,(format "hash-prime-%d" i))
       ("seed" . 123)
       ("moment_type" . "prime")
       ("observed_at" . (("tick" . ,i) ("monotonic_ms" . 0))))))
  
  ;; 3. Fork Alpha at Tick 5
  (dragonruby-stargate-session-fork "prime" 5 "alpha")
  
  ;; 4. Verify Inheritance (pre-divergence)
  (let ((m3 (dragonruby-stargate-session-get-moment "alpha" 3)))
    (unless (string= (cdr (assoc "hash" m3)) "hash-prime-3")
      (error "Inheritance Failed: alpha@3 should be prime@3, but got %S" m3)))
  
  ;; 5. Verify Isolation (post-divergence)
  (let ((m7 (dragonruby-stargate-session-get-moment "alpha" 7)))
    (if m7 (error "Isolation Failed: alpha@7 should be empty before recording")))
  
  ;; 6. Fill Alpha (Ticks 6-9)
  (dolist (i '(6 7 8 9))
    (dragonruby-stargate-session-record-moment 
     `(("address" . ,(format "alpha@%d" i))
       ("hash" . ,(format "hash-alpha-%d" i))
       ("seed" . 123)
       ("moment_type" . "prime")
       ("observed_at" . (("tick" . ,i) ("monotonic_ms" . 0))))))
  
  ;; 7. Verify Local lookup
  (let ((m7 (dragonruby-stargate-session-get-moment "alpha" 7)))
    (unless (string= (cdr (assoc "hash" m7)) "hash-alpha-7")
      (error "Local lookup Failed: alpha@7 should be hash-alpha-7")))
  
  ;; 8. Verify Shadowing (alpha@3 shadows prime@3)
  (dragonruby-stargate-session-record-moment 
   `(("address" . "alpha@3")
     ("hash" . "hash-alpha-shadow-3")
     ("seed" . 123)
     ("moment_type" . "prime")
     ("observed_at" . (("tick" . 3) ("monotonic_ms" . 0)))))
  
  (let ((m3 (dragonruby-stargate-session-get-moment "alpha" 3)))
    (unless (string= (cdr (assoc "hash" m3)) "hash-alpha-shadow-3")
      (error "Shadowing Failed: alpha@3 should shadow prime@3")))

  (message "✅ Stargate Inheritance & Shadowing: PASSED")
  (dragonruby-stargate-session-stop))

(test-stargate-inheritance)
