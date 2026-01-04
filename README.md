# DragonRuby Emacs Mode 🐉

![DragonRuby Emacs Mode](docs/emacs-image.jpg)

> **"Code is clay. This is your forge."**
> A high-performance semantic interface for DragonRuby developers who demand absolute control over their creative environment.

---

## 🏛️ The Manifesto: Beyond Text, Towards Meaning

Standard editors treat code as dead weight: a sea of gray strings and silent variables. **DragonRuby Emacs Mode** breaks this illusion. We believe your editor should be a **living conduit** between your thoughts and your game.

---

## ⚔️ The Stable Arsenal (The Foundational Base)

### 🎨 Level 1: Semantic Spectrum (Colors)
Visualize the invisible. Your game's palette, always present.
- **Visual Swatches**: High-contrast icons that scale with your environment.
- **Precise Technical Detection**: Support for Hex literals (`0xFF00FF`), RGB/RGBA Arrays `[255, 0, 0]`, and Hashes `{r: 255, g: 0, b:0}`.
- **Smart Guards**: Prevents accidental edits on visual markers while keeping code accessible.

### 🖼️ Level 2: Reconnaissance Operations (Sprites)
Stop coding blind. See your world as you build it.
- **Inline Intelligence**: High-definition thumbnails embedded directly in your code.
- **The Validation Law**: Cyan = Ready, Red = Missing, Orange = Illegal Format.
- **"Double C-M-i" Workflow** (CAPF):
  - Type `spr` + `C-M-i` → Expands to `""` (cursor inside).
  - Press `C-M-i` again → The minibuffer shows **only valid sprites** from your project.
  - **Without plugin**: You'd see all project files mixed together.
  - **With plugin**: You see only files from the `sprites/` folder with valid extensions (PNG, BMP, JPG, etc.).

### 🎨 The Forge: Integrated Image Editor

Press `RET` or `C-c C-o` on any sprite in your code and the header-line transforms into a **complete workstation**. You'll have access to a professional arsenal with non-destructive timeline, tool groups (VIEW, TRANSFORM, COLOR, SYSTEM, CREATIVE), and over 15 operations: remove backgrounds, trim margins, pixel-perfect scaling (2x/0.5x), apply effects, rotate, invert, compress, and much more. All without leaving Emacs.

**→ [📖 Complete Image Editor Guide](docs/IMAGE_EDITOR.md)** (Timeline, Tool groups, Creative Hub)


### ⚡ Level 3: Curvature Navigation (The Law)
Eliminate journey friction. Move at the speed of thought.
- **Contextual Intelligence**: The plugin knows when you're in a `require`, a `read_file`, or a `path:` assignment and filters your destinations accordingly.
- **Dimensional Links**: Every path is a portal. `C-c C-o` or `RET` to jump directly to any file or asset.
- **CAPF Completion**: Press `C-M-i` inside a string to see files of the correct type based on context.

---

## 💎 Tactical Arsenal (New in v0.5.0)

### 🔊 The Resonance Engine (Audio)
Audio components are no longer silent text. Feel your game's heartbeat.
- **Emerald Pulse**: Valid assets shine with vibrant green energy (`#2ECC71`).
- **Format Sentinel**: Instant suppression of weak formats. `.mp3` is marked and neutralized.
- **Chronicle Tooltips**: Instant file size information on hover.

### 🅰️ The Glyph Engine (Fonts)

Press `RET` on any font in your code and access a **complete interactive viewer**: test pangrams in 5 languages, review the full alphabet (ABCs), visualize custom text, and confirm each character renders correctly. Inline thumbnails adapt to your theme (dark/light) automatically, and the **Creative Hub** connects you with Google Fonts, DaFont, and your own personal repositories.

**→ [📖 Complete Font System Guide](docs/FONTS.md)** (ABC/Pangram Viewer, Typography resources, Troubleshooting)

### 🤝 Compatibility with Other Plugins

Designed to **not interfere** with your existing configuration:

| Plugin/System | Status | Notes |
|---------------|--------|-------|
| **LSP** (eglot, lsp-mode) | ✅ Stable | Non-exclusive CAPF, low priority (90-100) |
| **Company** | ✅ Stable | Compatible with `:exclusive 'no` |
| **Vertico/Ivy/Helm** | ✅ Stable | Doesn't affect minibuffer system |
| **Flycheck/Flymake** | ✅ Stable | Low priority overlays (-50) |
| **Evil Mode** | ✅ Stable | Doesn't modify standard keymaps |
| **Corfu** | 🧪 To test | Should work (same CAPF system) |
| **Doom Emacs** | 🧪 To test | Compatible with individual components |

💡 **Philosophy**: We add capabilities without replacing your workflow.

⚠️ **Community Testing**: This table is subject to continuous verification. If you find any incompatibility, **[report a bug on GitHub Issues](https://github.com/Macgyber/dragonruby-mode/issues)** so we can improve the plugin's stability.

---

## 📦 Installation

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

**→ [📖 Complete Installation Guide](docs/INSTALLATION.md)** (Spacemacs, use-package, package-vc, Quelpa, el-get, borg, manual)

---

## 🏗️ Architecture: The Iron Chassis

Built on a framework of **Fault-Tolerant Micro-Modules**. Each system is isolated, hardened, and independent.

| System | Deployment Level |
|--------|--------------------|
| `dragonruby-core` | 🧱 FOUNDATIONAL (Rock solid) |
| `dragonruby-colors`| ✅ DEPLOYED (Stable) |
| `dragonruby-sprites`| ✅ DEPLOYED (Stable) |
| `dragonruby-paths` | ✅ DEPLOYED (Stable) |
| `dragonruby-fonts` | 🟢 AWAKENING (New) |
| `dragonruby-audio` | 🟢 AWAKENING (New) |

---

## 📚 Documentation and Resources

- **[📖 IMAGE_EDITOR.md](docs/IMAGE_EDITOR.md)**: Complete image editor guide
- **[📖 FONTS.md](docs/FONTS.md)**: Font system and typography
- **[📖 INSTALLATION.md](docs/INSTALLATION.md)**: All installation methods
- **[🤝 CONTRIBUTING.md](docs/CONTRIBUTING.md)**: Guide for contributors
- **[💬 GitHub Issues](https://github.com/Macgyber/dragonruby-mode/issues)**: Report bugs or request features

---

*Forged for creators who refuse to settle. Forged for DragonRuby.*

**DragonRuby Emacs Mode — v0.5.0**

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
