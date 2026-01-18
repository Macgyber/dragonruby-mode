;;; dragonruby-stargate-injector.el --- Stargate Code Change Classifier -*- lexical-binding: t -*-

;; Author: Macgyber <esteban3261g@gmail.com>
;; Version: 0.8.0
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/Macgyber/dragonruby-mode

;;; Commentary:
;; This module classifies code changes into risk categories: 
;; Alpha (safe), Beta (risky), or Gamma (dangerous) as required by Law VI.

;;; Code:

(require 'dragonruby-stargate-bridge)

(defun dragonruby-stargate-injector-classify (snippet)
  "Classify the risk level of SNIPPET.
Returns a symbol: `alpha', `beta', or `gamma'."
  (cond
   ;; GAMMA: Structural changes (Class definitions, state key mutations)
   ((string-match-p (concat "\\<" (regexp-opt '("class" "module" "attr_accessor" "attr_reader" "attr_writer") t) "\\>") snippet)
    'gamma)
   
   ;; BETA: Functional logic changes (Methods, control flow)
   ((string-match-p (concat "\\<" (regexp-opt '("def" "if" "unless" "case" "while" "until" "loop") t) "\\>") snippet)
    'beta)
   
   ;; ALPHA: Pure data or simple expressions
   (t 'alpha)))

(defun dragonruby-stargate-injector-get-risk-details (category)
  "Return human-readable details for a risk CATEGORY."
  (cl-case category
    (gamma '(:label "GAMMA" :color "red" :desc "Structural Change: Dangerous. May require state reset."))
    (beta  '(:label "BETA"  :color "yellow" :desc "Logic Change: Risky. Monitor for side effects."))
    (alpha '(:label "ALPHA" :color "green" :desc "Data Change: Safe. Minor impact simulation."))))

(defun dragonruby-stargate-injector-transmit (snippet)
  "Classify and prepare SNIPPET for transmission to the Stargate Runtime."
  (let* ((category (dragonruby-stargate-injector-classify snippet))
         (details (dragonruby-stargate-injector-get-risk-details category)))
    (message "🧠 Stargate Injector: [%s] detected. %s" 
             (plist-get details :label) 
             (plist-get details :desc))
    ;; Transmit to the Body via the Bridge
    (dragonruby-stargate-bridge-send-code snippet)
    category))

(provide 'dragonruby-stargate-injector)
;;; dragonruby-stargate-injector.el ends here
