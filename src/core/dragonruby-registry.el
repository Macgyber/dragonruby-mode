;;; dragonruby-registry.el --- Concept Registry  -*- lexical-binding: t; -*-

(require 'dragonruby-core-concepts)

(defvar dragonruby--concepts (make-hash-table :test 'equal)
  "Registry of DragonRuby concepts indexed by concept id.")

(defun dragonruby-register-concept (concept)
  "Register a DragonRuby CONCEPT in the global registry."
  (puthash (dragonruby-concept-id concept)
           concept
           dragonruby--concepts))

(defun dragonruby-get-concept (id)
  "Retrieve a DragonRuby concept by ID."
  (gethash id dragonruby--concepts))

(defun dragonruby-all-concepts ()
  "Return a list of all registered DragonRuby concepts."
  (hash-table-values dragonruby--concepts))

(provide 'dragonruby-registry)
