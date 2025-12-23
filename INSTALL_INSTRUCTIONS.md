# Installation Instructions

To make **dragonruby-mode** available in every Emacs session, add the following code to your Emacs configuration file.

This file is usually located at:
- `~/.emacs`
- `~/.emacs.d/init.el`
- `~/.doom.d/config.el` (if using Doom Emacs)
- `~/.spacemacs` (inside `dotspacemacs/user-config`)

## Configuration Snippet

```elisp
;; --- DragonRuby Mode Configuration ---

;; 1. Add the installation directory to the load-path
(add-to-list 'load-path "~/emacs-packages/dragonruby-mode/src")

;; 2. Load the package
(require 'dragonruby)

;; 3. (Optional) Customize behavior if needed
;; (setq dragonruby-verbose t) 

;; ----------------------------------------
```

## Verify Installation

1. Restart Emacs or evaluate the code above (`M-x eval-buffer`).
2. Open any `.rb` file containing a DragonRuby `tick` method (e.g., one of the examples).
3. `dragonruby-mode` should activate automatically (look for "DR" in the modeline).
