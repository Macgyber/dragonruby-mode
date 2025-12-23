;;; dragonruby-args-sub.el --- Definitions for main subconcepts of args -*- lexical-binding: t; -*-

(require 'dragonruby-registry)

;; --- args.state ---
(dragonruby-register-concept
 (make-dragonruby-concept
  :id "args.state"
  :name "Game State (Memory)"
  :level 'core
  :scope 'state
  :definition "A dynamic OpenStruct where you store EVERYTHING that must persist between frames."
  :mental-model "The Memory Card. If it's not in .state, it is forgotten in the next frame."
  :presentation '((eldoc . t))))

;; --- args.outputs ---
(dragonruby-register-concept
 (make-dragonruby-concept
  :id "args.outputs"
  :name "Render Pipeline"
  :level 'core
  :scope 'render
  :definition "An ordered queue of arrays. What you push here gets drawn."
  :mental-model "The TV Screen. You don't 'draw', you 'queue' instructions for the GPU."
  :relations '(("contains" . "args.outputs.solids")
               ("contains" . "args.outputs.sprites"))
  :presentation '((eldoc . t))))

;; --- args.inputs ---
(dragonruby-register-concept
 (make-dragonruby-concept
  :id "args.inputs"
  :name "Input Hardware"
  :level 'core
  :scope 'input
  :definition "Read-only snapshots of Keyboard, Mouse, and Controller state."
  :mental-model "The Controller. You check it, you don't write to it."
  :presentation '((eldoc . t))))

(provide 'dragonruby-args-sub)
