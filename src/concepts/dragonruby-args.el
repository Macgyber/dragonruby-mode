;;; dragonruby-args.el --- Definition of the args concept -*- lexical-binding: t; -*-

(require 'dragonruby-registry)

(dragonruby-register-concept
 (make-dragonruby-concept
  :id "args"
  :name "Frame Arguments"
  :level 'core
  :scope 'frame
  :definition
  "The specific universe of data for the current 1/60th second of simulation."
  :intention
  "To centralize inputs, state, and outputs for each execution frame."
  :mental-model
  "Think of args as the Console itself:
   - Inputs: The Controller (what you press)
   - Outputs: The TV Screen (what you see)
   - State: The Memory Card (what is saved)"
  :problems
  '("Global state confusion"
    "Unclear input handling"
    "Unclear rendering pipeline")
  :limits
  '("Does not contain game logic"
    "Does not render by itself"
    "Does not persist data without state")
  :relations
  '(("contains" . "args.inputs")
    ("contains" . "args.state")
    ("contains" . "args.outputs"))
  :presentation
  '((eldoc . t)
    (tooltip . optional)
    (snippet . nil))
  :evolution
  "May gain sub-concepts, but its core definition must not change."))

(provide 'dragonruby-args)
