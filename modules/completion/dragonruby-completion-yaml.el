;;; dragonruby-completion-yaml.el --- Strict parser for Contract YAML -*- lexical-binding: t; -*-

(require 'cl-lib)

(defconst dragonruby-yaml--indent-step 2
  "Strictly enforced indentation step.")

(define-error 'dragonruby-yaml-error "DragonRuby Contract Error")

(defun dragonruby-yaml-parse-file (file)
  "Parse FILE as a strict DragonRuby Contract YAML.
Returns a hash-table representing the tree or nil on failure.
Follows the Fail-Safe policy: invalid YAML returns nil and warns once."
  (if (not (file-exists-p file))
      nil
    (with-temp-buffer
      (insert-file-contents file)
      (condition-case err
          (dragonruby-yaml--parse-buffer)
        (dragonruby-yaml-error
         (message "⚠️ [DragonRuby Contract] Ignored invalid YAML in %s: %s"
                  (file-name-nondirectory file)
                  (cdr err))
         nil)
        (error
         (message "⚠️ [DragonRuby Contract] Unexpected error parsing %s: %s"
                  (file-name-nondirectory file)
                  err)
         nil)))))

(defun dragonruby-yaml--parse-buffer ()
  "Parse current buffer as strict YAML lines.
Returns a hashtable."
  (goto-char (point-min))
  (let ((root (make-hash-table :test 'equal))
        (stack (list (cons -1 (make-hash-table :test 'equal))))) ;; Stack of (indent . object)
    
    ;; Root object is the top of the stack initially, but we need to handle top-level keys
    ;; Actually, simpler: The stack tracks the current open object.
    ;; We start with a dummy root at indent -1.
    
    (while (not (eobp))
      (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
             (trimmed (string-trim line)))
        (unless (or (string-empty-p trimmed)
                    (string-prefix-p "#" trimmed))
          ;; 1. Measure Indentation
          (let ((indent (- (length line) (length (string-trim-left line)))))
            
            ;; STRICT: Indentation must be multiple of 2
            (unless (zerop (% indent dragonruby-yaml--indent-step))
              (signal 'dragonruby-yaml-error 
                      (list (format "Line '%s' has invalid indentation (must be multiple of 2)" trimmed))))
            
            ;; 2. Parse Key-Value OR List Item
            ;; - Key-Value: "key:" or "key: value"
            ;; - List Item: "- value" (Referenced as Key=value, Value=t for set semantics)
            
            (let (key val)
              (cond
               ;; Case A: Standard Key (now supports $ and @)
               ((string-match "^\\([$@a-zA-Z0-9_]+\\):\\(?:\\s-*\\(.*\\)\\)?$" trimmed)
                (setq key (match-string 1 trimmed)
                      val (match-string 2 trimmed)))
               
               ;; Case B: List Item (Strict: "- value")
               ((string-match "^-\\s-*\\([a-zA-Z0-9_]+\\)$" trimmed)
                (setq key (match-string 1 trimmed)
                      val t)) ;; Use t to indicate presence/set membership
               
               (t
                (signal 'dragonruby-yaml-error 
                        (list (format "Line '%s' is not a valid map entry or list item" trimmed)))))
            
            ;; (message "DEBUG: Parsing Line: '%s' -> Key: '%s' Val: '%s'" trimmed key val)

              
              ;; 3. Manage Stack
              ;; Pop until we find the parent (indent < current)
              (while (>= (caar stack) indent)
                (pop stack))
              
              (let ((parent-obj (cdar stack))
                    (new-obj (if (or (not val) (string-empty-p val))
                                 (make-hash-table :test 'equal) ;; It's a node
                               val))) ;; It's a leaf/value
                
                ;; Add to parent
                (puthash key new-obj parent-obj)
                
                ;; If it's a node, push to stack
                (when (hash-table-p new-obj)
                  (push (cons indent new-obj) stack)))))))
      
      (forward-line 1))
    
    ;; Return the content of the dummy root
    (cdar (last stack)))) 

;; Introspection Helper
(defun dragonruby-yaml-inspect (table &optional indent)
  "Return a string debug representation of TABLE."
  (let ((result "")
        (ind (make-string (or indent 0) ?\s)))
    (if (hash-table-p table)
        (maphash (lambda (k v)
                   (setq result (concat result 
                                        (format "%s%s:\n%s" 
                                                ind k 
                                                (dragonruby-yaml-inspect v (+ (or indent 0) 2))))))
                 table)
      (format "%s  %s\n" ind table))
    result))

(provide 'dragonruby-completion-yaml)
;;; dragonruby-completion-yaml.el ends here
