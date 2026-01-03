;;; dragonruby-sprite-model.el --- Domain logic for sprites -*- lexical-binding: t; -*-

(defvar dragonruby-supported-sprites '("png" "jpg" "jpeg" "gif" "bmp")
  "Extensions supported by DragonRuby Game Toolkit.")

(defvar dragonruby-unsupported-sprites '("svg" "psd" "xcf" "ase" "aseprite")
  "Extensions that are common but NOT supported by DragonRuby.")

(provide 'dragonruby-sprite-model)
;;; dragonruby-sprite-model.el ends here
