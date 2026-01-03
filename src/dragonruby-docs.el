;;; dragonruby-docs.el --- Documentation linkage and lookup -*- lexical-binding: t; -*-

(require 'dragonruby-core)
(require 'org)

(defvar dragonruby-docs-directory
  (let ((current-dir (file-name-directory (or load-file-name buffer-file-name))))
    (expand-file-name "../docs/concepts" current-dir))
  "Directory containing local DragonRuby documentation concepts.")

;; --- FEATURE FLAGS ---

(defcustom dragonruby-experimental-concepts-docs nil
  "Enable experimental concept documentation lookup system via .org files.
Refers to the \\='Living Documentation\\=' system."
  :type 'boolean
  :group 'dragonruby)

;; --- REGISTRY PARSING ---

(defvar dragonruby-docs--concept-map nil
  "Hash table mapping search terms to docs filenames, loaded from index.org.")

(defun dragonruby-docs--parse-registry ()
  "Parse docs/concepts/index.org and populate `dragonruby-docs--concept-map`."
  (let ((index-file (expand-file-name "index.org" dragonruby-docs-directory))
        (map (make-hash-table :test 'equal)))
    (when (file-exists-p index-file)
      (with-temp-buffer
        (insert-file-contents index-file)
        (goto-char (point-min))
        (while (re-search-forward "^\\* \\(.*\\)" nil t)
          (let ((_current-concept (match-string 1)))
            ;; Find properties block
            (when (re-search-forward ":PROPERTIES:" nil t)
              (let ((doc-file nil)
                    (terms nil))
                (save-excursion
                  (when (re-search-forward ":DOC: \\(.*\\)" nil t)
                    (setq doc-file (match-string 1)))
                  (goto-char (match-beginning 0)) ;; Back to properties
                  (when (re-search-forward ":SEARCH_TERMS: \\(.*\\)" nil t)
                    (setq terms (split-string (match-string 1) " " t))))
                
                (when (and doc-file terms)
                  (dolist (term terms)
                    (puthash term doc-file map)))))))))
    (setq dragonruby-docs--concept-map map)))

(defun dragonruby-docs--find-concept-for-symbol (symbol)
  "Find the documentation file for SYMBOL using the registry map."
  (unless dragonruby-docs--concept-map
    (dragonruby-docs--parse-registry))
  (gethash symbol dragonruby-docs--concept-map))

(defun dragonruby-docs-open (filename &optional anchor)
  "Open documentation FILENAME (e.g. sprite.org).
If ANCHOR is provided, it is treated as a semantic intent (e.g. \\='definition\\=',
\\='example\\='). The function searches for a :NAV-TARGET: property matching ANCHOR.
Falls back to searching for a header matching ANCHOR title if no property found."
  (let ((path (expand-file-name filename dragonruby-docs-directory)))
    (if (file-exists-p path)
        (progn
          (find-file path)
          (when anchor
            (goto-char (point-min))
            ;; 1. Try Semantic Property Search (:NAV-TARGET: anchor)
            (let ((target-found nil))
              (while (and (not target-found) (re-search-forward (format ":NAV-TARGET: +%s" (regexp-quote anchor)) nil t))
                (save-excursion
                  (if (re-search-backward "^\\*+ " nil t)
                      (setq target-found (point))
                    (setq target-found nil))))
              
              (if target-found
                  (progn
                    (goto-char target-found)
                    (recenter-top-bottom 0)
                    (if (fboundp 'org-fold-show-entry)
                        (org-fold-show-entry)
                      (with-no-warnings (org-show-entry))))
                
                ;; 2. Fallback: Search for Header Title exact match
                (goto-char (point-min))
                (if (re-search-forward (format "^\\*+ %s" (regexp-quote anchor)) nil t)
                    (progn
                      (recenter-top-bottom 0)
                      (if (fboundp 'org-fold-show-entry)
                          (org-fold-show-entry)
                        (with-no-warnings (org-show-entry))))
                  (message "⚠️ Section for intention '%s' not found in %s" anchor filename)))))
          (message "📖 Opened knowledge node: %s" filename))
      (message "❌ Documentation file missing: %s" filename))))

(defun dragonruby-docs-visit-at-point ()
  "Intelligently open documentation for the concept at point.
Queries the `index.org` registry to match symbols to concepts.
Defaults to showing the \\='definition\\=' section."
  (interactive)
  (if (not dragonruby-experimental-concepts-docs)
      (dragonruby--warn-in-development "Knowledge System (Docs)")
    (let* ((symbol (thing-at-point 'symbol t))
           (concept-file (when symbol (dragonruby-docs--find-concept-for-symbol symbol))))
      
      (if concept-file
          (dragonruby-docs-open concept-file "definition")
        ;; Fallback: Try exact filename match or show all concepts
        (let* ((all-concepts (when dragonruby-docs--concept-map (hash-table-keys dragonruby-docs--concept-map)))
               (selection (completing-read (format "No exact match for '%s'. Search concept: " (or symbol "")) 
                                           all-concepts)))
          (when selection
             (dragonruby-docs-open (gethash selection dragonruby-docs--concept-map) "definition")))))))

(define-minor-mode dragonruby-docs-mode
  "Minor mode to enable documentation lookups."
  :lighter ""
  :group 'dragonruby
  ;; Keymaps or setup could go here
  )

(provide 'dragonruby-docs)
;;; dragonruby-docs.el ends here
