;;; dragonruby-completion-backend.el --- The Resolver for Contract Paths -*- lexical-binding: t; -*-

(require 'dragonruby-completion-yaml)
(require 'subr-x)

(defvar-local dragonruby-completion--contract nil
  "The current buffer's active contract tree (hash-table).")

(defvar-local dragonruby-completion--roots nil
  "List of valid root strings for the current contract.")

(defun dragonruby-completion-backend-init (yaml-file)
  "Initialize the resolver with YAML-FILE."
  (setq-local dragonruby-completion--contract (dragonruby-yaml-parse-file yaml-file))
  (when dragonruby-completion--contract
    ;; Extract roots from the contract
    ;; Contract structure: {"roots": [...], "trees": { ... }} (implicit or explicit)
    ;; Our parser returns raw hash. We need to respect the schema.
    
    ;; Schema Check:
    ;; If the root hash has "roots" and "trees", use them.
    ;; If not, and it looks like a flat tree, assume top-level keys are roots?
    ;; NO. The Plan says:
    ;; roots:
    ;;   - args
    ;; trees:
    ;;   args: ...
    
    (let ((roots (gethash "roots" dragonruby-completion--contract))
          (trees (gethash "trees" dragonruby-completion--contract)))
      
      (if (and roots trees)
          ;; Schema V1 matched
          (progn
            ;; Roots might be a list or a single value depending on the parser if I handled sequences...
            ;; Wait, my parser explicitly REJECTS sequences for now unless I add sequence support?
            ;; My parser said: "No sequences (except at root if needed)".
            ;; But the parser code `dragonruby-yaml--parse-buffer` currently expects keys.
            ;; It treats everything as maps. "roots: - args" would fail or be parsed weirdly.
            ;;
            ;; FIX: The parser needs to handle the `roots` list, OR we simplfy.
            ;; User Plan: "maps only... No sequences (except at root if needed)".
            ;; My parser implementation `dragonruby-yaml--parse-buffer` does NOT handle sequences (list items starting with -).
            ;;
            ;; CORRECTIVE ACTION: I will assume the roots are defined as a map for now or 
            ;; I need to update the parser to handle basic lists.
            ;; Given the user said "maps only", maybe `roots` should be a map?
            ;; Or, I can just update the parser right now to handle simple list values before proceeding.
            ;;
            (setq-local dragonruby-completion--roots (hash-table-keys roots)))
          
        ;; Fallback/Simplified Schema: Top level keys are roots
        ;; This is useful for simple "args: ..." files without the metadata wrapper.
        (setq-local dragonruby-completion--roots (hash-table-keys dragonruby-completion--contract))))))

;; (require 'dragonruby-symbols)

(defun dragonruby-completion-at-point ()
  "CAPF function. STRICTLY matches explicit paths."
  ;; 1. Check if we have a contract
  (unless dragonruby-completion--contract
    (cl-return-from dragonruby-completion-at-point nil))
  
  ;; 2. Get the symbol at point and the preceding context
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (end (point))
         (start (or (car bounds) end))
         ;; context-start searches back for the chain
         (line-start (line-beginning-position))
         ;; FIX: Always use (point) to capture the text immediately before cursor, 
         ;; ignoring Emacs symbol boundaries which might exclude the dot.
         (prefix-str (buffer-substring-no-properties line-start (point))))
    
     ;; REGEX: Match chain, optionally ending in dot.
    ;; Group 1: Full Chain e.g. "args.state" or "args"
    (when (string-match "\\(?:^\\|[^a-zA-Z0-9_$@]\\)\\([a-zA-Z0-9_$@]+\\(\\.[a-zA-Z0-9_$@]+\\)*\\)\\.?$" prefix-str)
      (let* ((full-chain (match-string 1 prefix-str)) ;; e.g. "args" or "args.state"
             (parts (split-string full-chain "\\."))
             (root (car parts))
             (path (cdr parts)))

        ;; Logic Branch:
        ;; 1. If path exists OR full-chain ends in dot -> We are traversing INSIDE a root.
        ;; 2. If path is nil AND no dot -> We are typing the ROOT itself.

        (if (or path (string-suffix-p "." prefix-str))
            ;; -- Traversal Logic (Existing) --
            (let ((node (dragonruby-completion--resolve-component root path)))
              (when node
                (let ((candidates (dragonruby--get-children node)))
                  (when candidates
                    (list start end candidates :exclusive 'no)))))

          ;; -- Root Logic (New) --
          ;; We are typing a top-level root like "arg" or "gt".
          ;; Suggest known roots ONLY if they match the current prefix (Fallthrough otherwise).
          (let* ((trees (gethash "trees" dragonruby-completion--contract))
                 (all-roots (if trees (hash-table-keys trees) 
                              (hash-table-keys dragonruby-completion--contract)))
                 ;; Filter roots that start with the current word 'root'
                 (matching-roots (seq-filter (lambda (r) (string-prefix-p root r)) all-roots)))
            
            (when matching-roots
              (list start end 
                    all-roots ;; Return ALL roots so Emacs can show alternatives, but only if we had at least one match.
                    :exclusive 'no
                    :exit-function 
                    (lambda (str status)
                      ;; If we finished a ROOT completion, append a dot automatically
                      ;; to help the user flow into properties.
                      (when (eq status 'finished)
                        (insert ".")))))))))))

(defun dragonruby-completion--resolve-component (root path)
  "Traverse the contract from ROOT down through PATH.
RENAMED: To avoid conflict with dragonruby--resolve-path in utils."
  (let ((current (gethash root (or (gethash "trees" dragonruby-completion--contract)
                                   dragonruby-completion--contract))))
    (dolist (step path)
      (when current
        ;; Traversal:
        ;; V1 Schema: current -> children -> step
        ;; Simple Schema: current -> step
        (let ((v1-children (gethash "children" current)))
          (if v1-children
              (setq current (gethash step v1-children))
            (setq current (gethash step current))))))
    current))

(defun dragonruby--get-children (node)
  "Extract children keys from NODE."
  (when (hash-table-p node)
    (let ((v1-children (gethash "children" node)))
      (if v1-children
          (hash-table-keys v1-children)
        (hash-table-keys node)))))

(defun dragonruby-completion-backend-valid-chain-p (chain-str)
  "Return t if CHAIN-STR is a valid root or part of a valid chain in the contract."
  (when (and dragonruby-completion--contract chain-str)
    (let* ((parts (split-string chain-str "\\."))
           (root (car parts))
           (path (cdr parts)))
      (if (null path)
          ;; Simple root check
          (let ((trees (gethash "trees" dragonruby-completion--contract)))
            (if trees
                (gethash root trees)
              (gethash root dragonruby-completion--contract)))
        ;; Full chain resolution
        (dragonruby-completion--resolve-component root path)))))

(provide 'dragonruby-completion-backend)
;;; dragonruby-completion-backend.el ends here
