;;; dragonruby-registry.el --- Central concept registry -*- lexical-binding: t; -*-

;; The Census of the World.
;; Maps domain concepts (sprites, etc.) to their metadata and handlers.

(defvar dragonruby-registry-table (make-hash-table :test 'eq)
  "Storage for registered concepts.")

(defun dragonruby-registry-register (id props)
  "Register a concept ID with PROPS (plist).
Example:
(dragonruby-registry-register \\='sprite
  \\='(:doc \"docs/concepts/sprite.org\"
    :overlay dragonruby-sprite-overlay))"
  (puthash id props dragonruby-registry-table))

(defun dragonruby-registry-get (id prop)
  "Retrieve PROP for concept ID."
  (plist-get (gethash id dragonruby-registry-table) prop))

(defun dragonruby-registry-all-ids ()
  "Return list of all registered concept IDs."
  (hash-table-keys dragonruby-registry-table))

(provide 'dragonruby-registry)
