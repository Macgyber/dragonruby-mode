# Complete Installation Guide 🚀

**DragonRuby Emacs Mode** supports **all Emacs package managers and configurations**. Choose the method that best fits your workflow.

---

## 🎯 Quick Methods (Recommended)

### Doom Emacs
```elisp
;; packages.el
(package! dragonruby-mode :recipe (:host github :repo "Macgyber/dragonruby-mode"))

;; config.el
(use-package! dragonruby-mode
  :hook ((ruby-mode . dragonruby-maybe-enable)
         (ruby-ts-mode . dragonruby-maybe-enable)))
```

### straight.el
```elisp
(straight-use-package
 '(dragonruby-mode :type git :host github :repo "Macgyber/dragonruby-mode"))

(use-package dragonruby-mode
  :straight t
  :hook ((ruby-mode . dragonruby-maybe-enable)
         (ruby-ts-mode . dragonruby-maybe-enable)))
```

---

## 🌟 Other Supported Methods

### Spacemacs
```elisp
;; dotspacemacs/layers
(setq-default dotspacemacs-additional-packages
              '((dragonruby-mode :location (recipe :fetcher github
                                                    :repo "Macgyber/dragonruby-mode"))))

;; dotspacemacs/user-config
(use-package dragonruby-mode
  :hook ((ruby-mode . dragonruby-maybe-enable)
         (ruby-ts-mode . dragonruby-maybe-enable)))
```

### use-package (Vanilla Emacs)
```elisp
(use-package dragonruby-mode
  :ensure t
  :vc (:fetcher github :repo "Macgyber/dragonruby-mode")
  :hook ((ruby-mode . dragonruby-maybe-enable)
         (ruby-ts-mode . dragonruby-maybe-enable)))
```

### package-vc (Emacs 29.1+)
```elisp
(package-vc-install "https://github.com/Macgyber/dragonruby-mode")

;; Luego añade a tu init.el:
(add-hook 'ruby-mode-hook #'dragonruby-maybe-enable)
(add-hook 'ruby-ts-mode-hook #'dragonruby-maybe-enable)
```

### Quelpa
```elisp
(quelpa '(dragonruby-mode :fetcher github :repo "Macgyber/dragonruby-mode"))

(use-package dragonruby-mode
  :hook ((ruby-mode . dragonruby-maybe-enable)
         (ruby-ts-mode . dragonruby-maybe-enable)))
```

### el-get
```elisp
(el-get-bundle Macgyber/dragonruby-mode)

(use-package dragonruby-mode
  :hook ((ruby-mode . dragonruby-maybe-enable)
         (ruby-ts-mode . dragonruby-maybe-enable)))
```

### borg (Emacs Collective)
```bash
cd ~/.emacs.d/.emacs.d/lib
git submodule add https://github.com/Macgyber/dragonruby-mode.git
git submodule init
```

```elisp
(use-package dragonruby-mode
  :hook ((ruby-mode . dragonruby-maybe-enable)
         (ruby-ts-mode . dragonruby-maybe-enable)))
```

---

## 🔮 Coming Soon

### MELPA
```elisp
;; Cuando esté disponible en MELPA:
(use-package dragonruby-mode
  :ensure t
  :hook ((ruby-mode . dragonruby-maybe-enable)
         (ruby-ts-mode . dragonruby-maybe-enable)))
```

---

## ⚙️ Manual Installation

### git clone
```bash
cd ~/.emacs.d/
git clone https://github.com/Macgyber/dragonruby-mode.git
```

```elisp
;; Add to your init.el:
(add-to-list 'load-path "~/.emacs.d/dragonruby-mode")
(require 'dragonruby-mode)
(add-hook 'ruby-mode-hook #'dragonruby-maybe-enable)
(add-hook 'ruby-ts-mode-hook #'dragonruby-maybe-enable)
```

### Direct Download (ZIP)
1. Download the [ZIP file](https://github.com/Macgyber/dragonruby-mode/archive/refs/heads/main.zip)
2. Extract to `~/.emacs.d/dragonruby-mode/`
3. Follow the "git clone" instructions above

---

## 🛠️ Post-Installation Setup

### Automatic Activation
The plugin activates automatically when it detects a DragonRuby project (looks for `app/main.rb`).

### Manual Activation
If you want to force activation on any Ruby file:
```elisp
M-x dragonruby-mode
```

### Disable in Specific Projects
```elisp
;; In .dir-locals.el of your regular Ruby project:
((ruby-mode . ((eval . (dragonruby-mode -1)))))
```

---

## 🐛 Troubleshooting

### Plugin doesn't load
1. **Check installation**: `M-x list-packages` (look for dragonruby-mode)
2. **Check load-path**: `C-h v load-path`
3. **Review startup errors**: `*Messages*` buffer

### Doesn't activate automatically
- **Check you're in a DragonRuby project** (`app/main.rb` must exist)
- **Activate manually**: `M-x dragonruby-mode`

### Conflicts with other modes
- Review the **Compatibility** section in the main README
- [Report incompatibilities](https://github.com/Macgyber/dragonruby-mode/issues)

---

## 📚 Additional Resources

- **[Main README](../README.md)**: Features and philosophy
- **[Compatibility](../README.md#-compatibility-with-other-plugins)**: Supported plugins table
- **[Contributing](CONTRIBUTING.md)**: Guide for contributors

---

**DragonRuby Emacs Mode — Universal Installation v0.7.2**
