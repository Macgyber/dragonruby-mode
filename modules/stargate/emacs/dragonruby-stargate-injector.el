;;; dragonruby-stargate-injector.el --- Stargate Code Change Classifier -*- lexical-binding: t -*-

;; Author: Macgyber <esteban3261g@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/Macgyber/dragonruby-mode

;;; Commentary:
;; This module classifies code changes into risk categories: 
;; Alpha (safe), Beta (risky), or Gamma (dangerous) as required by Law VI.
;; Hardened for v1.0 Blindada: Meta-programming detection and risk-aware injection.

;;; Code:

(require 'dragonruby-stargate-bridge)

(defun dragonruby-stargate-injector-classify (snippet)
  "Classify the risk level of SNIPPET.
Conservative: default to gamma if suspicious meta-programming or structural changes are found."
  (cond
   ;; GAMMA: Structural changes and Dynamic meta-programming
   ((string-match-p (concat "\\<" (regexp-opt '("class" "module" 
                                                "attr_accessor" "attr_reader" "attr_writer"
                                                "eval" "instance_eval" "class_eval" "module_eval"
                                                "send" "public_send" "define_method" 
                                                "define_singleton_method" "const_set" "remove_const") t) "\\>") snippet)
    'gamma)
   
   ;; BETA: Functional logic changes (Methods, control flow)
   ((string-match-p (concat "\\<" (regexp-opt '("def" "if" "unless" "case" "while" "until" "loop") t) "\\>") snippet)
    'beta)
   
   ;; ALPHA: Pure data or simple expressions
   (t 'alpha)))

(defun dragonruby-stargate-injector-get-risk-details (category)
  "Return human-readable details for a risk CATEGORY."
  (cl-case category
    (gamma '(:label "GAMMA" :color "red" :desc "Structural/Meta Change: Irreversible. Proceed with caution."))
    (beta  '(:label "BETA"  :color "yellow" :desc "Logic Change: Dynamic method update. Reversible."))
    (alpha '(:label "ALPHA" :color "green" :desc "Data/Value Change: Safe expression update."))))

(defun dragonruby-stargate-injector-transmit (snippet)
  "Classify and transmit SNIPPET to the Stargate Runtime with intent."
  (let* ((category (dragonruby-stargate-injector-classify snippet))
         (details (dragonruby-stargate-injector-get-risk-details category))
         ;; Wrap in a risk-aware injection call (Law VI Integrity)
         (wrapped-code (format "Stargate::Injection.inject!(%S, risk: :%s)" snippet category)))
    
    (message "🧠 Stargate Injector: [%s] detected. %s" 
             (plist-get details :label) 
             (plist-get details :desc))
    
    ;; GAMMA protection: Warn the user if it's a dangerous change
    (when (and (eq category 'gamma) (called-interactively-p 'interactive))
      (unless (y-or-n-p "⚠️ STARGATE: Gamma change detected (irreversible). Inject anyway? ")
        (error "Injection aborted by user")))

    ;; Transmit to the Body via the Bridge using the hardened payload
    (dragonruby-stargate-bridge-send-code wrapped-code)
    category))

(provide 'dragonruby-stargate-injector)
;;; dragonruby-stargate-injector.el ends here
