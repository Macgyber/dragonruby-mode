# Manual Installation Guide

This guide contains detailed, OS-specific instructions for manually installing `dragonruby-mode`. 

> 💡 **Note**: We recommend using your preferred Emacs package manager (Straight, Doom, etc.) if possible. This guide is for those who need manual configuration.

## Configure Emacs

### 📍 Finding Your Path
You need to know where you cloned `dragonruby-mode`.

| Where you saved it | Your path would be |
|-------------------|-------------------|
| Desktop (Windows) | `C:/Users/YourName/Desktop/dragonruby-mode` |
| Downloads (Windows) | `C:/Users/YourName/Downloads/dragonruby-mode` |
| Home (Linux/Mac) | `~/dragonruby-mode` |
| Projects folder | `~/projects/dragonruby-mode` |
| Custom location | Wherever you ran `git clone` |

### 🪟 Windows

1. Press `Win + R` to open Run dialog
2. Type `%appdata%` and press Enter
3. Scroll down to find the `.emacs` file (it's in the root, not in a folder)
4. Open it with any text editor (Notepad, VS Code, etc.)
5. Add this configuration at the end:

```elisp
;; DragonRuby Mode
;; ⚠️ CHANGE THIS PATH to where YOU saved dragonruby-mode!
;; Examples:
;;   "C:/Users/YourName/Desktop/dragonruby-mode"
;;   "C:/Users/YourName/Downloads/dragonruby-mode"
;;   "D:/projects/dragonruby-mode"
(add-to-list 'load-path "C:/Users/YourName/Desktop/dragonruby-mode")
(require 'dragonruby-mode)

;; Smart activation: only enables in DragonRuby projects
(add-hook 'ruby-mode-hook #'dragonruby-maybe-enable)
(add-hook 'ruby-ts-mode-hook #'dragonruby-maybe-enable)
```

6. Save and restart Emacs

> ⚠️ **Use forward slashes `/` in paths, not backslashes `\`**

### 🐧 Linux

1. Open a terminal
2. Edit your config file:

```bash
# Modern Emacs (29+)
nano ~/.config/emacs/init.el

# Traditional Emacs
nano ~/.emacs
```

3. Add this configuration:

```elisp
;; DragonRuby Mode
;; ⚠️ CHANGE THIS PATH to where YOU saved dragonruby-mode!
;; Examples:
;;   "~/dragonruby-mode"              (if in home folder)
;;   "~/Downloads/dragonruby-mode"    (if in Downloads)
;;   "~/projects/dragonruby-mode"     (if in projects folder)
(add-to-list 'load-path "~/Downloads/dragonruby-mode")
(require 'dragonruby-mode)

;; Smart activation: only enables in DragonRuby projects
(add-hook 'ruby-mode-hook #'dragonruby-maybe-enable)
(add-hook 'ruby-ts-mode-hook #'dragonruby-maybe-enable)
```

4. Save (`Ctrl+O`, `Enter`) and exit (`Ctrl+X`)
5. Restart Emacs

### 🍎 macOS

1. Open Terminal
2. Edit your config file:

```bash
# Using nano
nano ~/.emacs

# Or for modern Emacs
nano ~/.config/emacs/init.el
```

3. Add this configuration:

```elisp
;; DragonRuby Mode
;; ⚠️ CHANGE THIS PATH to where YOU saved dragonruby-mode!
;; Examples:
;;   "~/dragonruby-mode"              (if in home folder)
;;   "~/Downloads/dragonruby-mode"    (if in Downloads)
;;   "/Users/YourName/Desktop/dragonruby-mode"
(add-to-list 'load-path "~/Downloads/dragonruby-mode")
(require 'dragonruby-mode)

;; Smart activation: only enables in DragonRuby projects
(add-hook 'ruby-mode-hook #'dragonruby-maybe-enable)
(add-hook 'ruby-ts-mode-hook #'dragonruby-maybe-enable)
```

4. Save and restart Emacs

## Troubleshooting

### 🔍 Finding Your Config from Inside Emacs

If you're unsure which config file Emacs is using:

1. Press `M-:` (Alt + Shift + ;)
2. Type: `(find-file user-init-file)`
3. Press Enter

This opens the exact file Emacs is reading on startup.

### 🔍 Finding Where You Saved dragonruby-mode

If you forgot where you cloned the repository:

**Windows:**
1. Open File Explorer
2. Search for `dragonruby-mode.el`
3. The folder containing this file is your path

**Linux/macOS:**
```bash
find ~ -name "dragonruby-mode.el" 2>/dev/null
```
