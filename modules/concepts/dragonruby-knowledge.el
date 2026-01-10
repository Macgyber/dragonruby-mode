;;; dragonruby-knowledge.el --- The Brain of the Concept System -*- lexical-binding: t; -*-

;; Author: DragonRuby Emacs Team
;; Version: 0.6.0
;; Keywords: docs, learning, concepts

;;; Commentary:
;; This file acts as the knowledge database. It maps keywords to "Knowledge Cards".
;; Designed to support the "Michael Course" educational vision.

;;; Code:

(defvar dragonruby-knowledge-db
  '(("tick" . "❤️ The Heartbeat.\nRunning 60 times per second, this method drives your game logic.\n[args] contains the world state.")
    
    ("args" . "📦 The World Container.\nA global object passed to tick. Holds everything:\n- inputs (keyboard/mouse)\n- outputs (render)\n- state (data)")
    
    ("state" . "🧠 Persistent Memory.\nStore your game data here (player_hp, score).\nData survives hot-reloading.")
    
    ("outputs" . "🎨 The Canvas.\nSend arrays here to draw them.\n- outputs.solids << [x, y, w, h, r, g, b]\n- outputs.sprites << [x, y, w, h, 'path.png']")
    
    ("inputs" . "🎮 Control Center.\nRead keyboard, mouse, and controller state.\n- inputs.keyboard.key_down.space\n- inputs.mouse.click")
    
    ("grid" . "📏 Coordinate System.\n1280x720 logical resolution.\nCenter: 640, 360.\nBottom-Left: 0, 0.")
    
    ("attr_sprite" . "🖼️ Sprite Primitive.\nA Hash or Array representing an image.\nRequired: x, y, w, h, path."))
  "Alist mapping technical terms to beginner-friendly explanations.")

(defun dragonruby-knowledge-get (term)
  "Retrieve the explanation for a given TERM."
  (cdr (assoc term dragonruby-knowledge-db)))

(provide 'dragonruby-knowledge)
;;; dragonruby-knowledge.el ends here
