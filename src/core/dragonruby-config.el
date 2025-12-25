;;; dragonruby-config.el --- User Configuration Variables -*- lexical-binding: t; -*-

(defgroup dragonruby nil
  "DragonRuby development assistance."
  :group 'tools
  :prefix "dragonruby-")

(defcustom dragonruby-enable-color-preview t
  "Enable live color preview overlays for [r,g,b,a,x] arrays."
  :type 'boolean
  :group 'dragonruby)

(defcustom dragonruby-enable-sprite-preview t
  "Enable sprite path underlining and hover previews."
  :type 'boolean
  :group 'dragonruby)

(defcustom dragonruby-enable-require-linking t
  "Enable clickable require statements."
  :type 'boolean
  :group 'dragonruby)

(defcustom dragonruby-enable-auto-completion t
  "Enable automatic completion trigger when typing '/' in strings."
  :type 'boolean
  :group 'dragonruby)

(defcustom dragonruby-max-overlays-per-type 50
  "Maximum number of overlays of each type to create in a buffer.
Prevents visual overload in files with many colors/sprites."
  :type 'integer
  :group 'dragonruby)

(defcustom dragonruby-tooltip-delay 0.1
  "Delay in seconds before showing tooltips (sprites, colors).
Default is 0.1s for rapid feedback."
  :type 'float
  :group 'dragonruby)

(provide 'dragonruby-config)
