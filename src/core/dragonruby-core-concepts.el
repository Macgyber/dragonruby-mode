;;; dragonruby-core-concepts.el --- Core DragonRuby concept definitions  -*- lexical-binding: t; -*-

(require 'cl-lib)

(cl-defstruct dragonruby-concept
  id          ;; symbol: args, args.inputs, etc.
  name        ;; human-readable name
  level       ;; core | basic | advanced
  scope       ;; frame | input | render | state
  definition  ;; one-line essential definition
  intention   ;; why this concept exists
  mental-model
  problems
  limits
  relations
  presentation
  evolution)

(provide 'dragonruby-core-concepts)
