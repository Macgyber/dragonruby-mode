# DragonRuby Emacs Mode 🐉

<div align="center">
  <img src="docs/emacs-image.jpg" width="50%" alt="DragonRuby Emacs Mode">
  <br><br>
  <b>"It wasn't supposed to be possible."</b><br>
  We built this not just to write code, but to feel it. A love letter to the DragonRuby community and the infinite potential of Emacs.
</div>

---

## 🏛️ The New Standard: A Quiet Revolution

For years, we accepted that "powerful" meant "complicated" and "fast" meant "ugly". We didn't agree.

**DragonRuby Emacs Mode** is a milestone. It is the result of obsessively asking: *"What if your editor felt less like a tool and more like an extension of your mind?"*

We haven't just added features; we have established an **Invisible Order**. From the zero-latency sprite rendering to the industrial-grade memory management, every line of code here exists to protect your flow. We don't want to receive the credit; we want you to create the impossible, and forget we are even here.

This is our contribution to the craft. This is the new baseline.

---

## ⚔️ The Stable Arsenal (The Foundational Base)

### 🎨 Level 1: Semantic Spectrum (Colors)
Visualize the invisible. Your game's palette, always present.
- **Visual Swatches**: High-contrast icons that scale with your environment.
- **Precise Technical Detection**: Support for Hex literals (`0xFF00FF`), RGB/RGBA Arrays `[255, 0, 0]`, and Hashes `{r: 255, g: 0, b:0}`.
- **Interactive Picker (Optional)**: Enable `dragonruby-enable-picker` to unlock the click-to-edit feature (`■`). By default, swatches are read-only to prevent accidental edits.

<div align="center">
  <img src="docs/colors-preview.png" alt="Color Detection Preview" width="60%">
  <br><em>Arrays, RGBA hashes, and hexadecimal — all detected and visualized.</em>
</div>

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

<div align="center">
  <img src="docs/forge-toolbar.png" alt="The Forge Toolbar" width="80%">
  <br><em>VIEW, TRANSFORM, COLOR, SYSTEM, CREATIVE — Five tool groups, infinite power.</em>
</div>

### 🌐 Level 4: The Creative Hub (New in v0.5.0)
Your bridge to the ecosystem. Located within the Image Editor header-line:
- **Instant Web Tools**: One-click access to **Graphite**, **Piskel**, **Lospec**, **Itch.io**, and the DragonRuby Discord.
- **Customizable**: Add your own tools (Web URLs or Local Executables) via the `[+]` button.
- **Adaptive**: Tools have custom colors and can be hidden/shown to fit your workflow.

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

**✨ Context Preview**: The plugin reads nearby `text:` values and uses them to render the font preview. Write `text: "Press Start"` near your font, and the thumbnail will show that exact phrase rendered in your font!

<div align="center">
  <img src="docs/font-context-preview.png" alt="Font Context Preview" width="60%">
  <br><em>The plugin reads "Pres Start" from the nearby text: and renders it in the font.</em>
</div>

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

### 🏭 The Industrial Standard (New in v0.6.0)
**"The visible speed is born from the invisible order."**

We have re-engineered the core to meet **SRE & Performance Engineering** standards:
- **Zero Blocking**: Sprite scanning is now instantaneous. Heavy image processing is strictly deferred to idle time (lazy loading).
- **Controlled Memory**: Implemented **Singleton Timer Patterns** to prevent resource leaks during rapid interaction.
- **Silent Core**: The system now handles invalid contexts (like scratchpads) with absolute silence, ensuring stability 100% of the time.

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

## 🏗️ Architecture: The Lego System (v0.7.0)
"Complexity is the enemy. Modularity is the shield."

We have re-forged the core into a **Lego Architecture**. The system is no longer a monolith; it is a collection of independent, intelligent pieces orchestrated by a central Kernel.

### 🧠 The Kernel
The Kernel is the central nervous system. It strictly enforces the **Three Laws**:
1.  **Namespace Law**: Every module owns its space. No collisions.
2.  **Capability Law**: Modules request powers (e.g. `:rendering`), and the Kernel finds the provider.
3.  **Cold Boot Law**: Nothing runs until you ask for it. Zero zombie processes.

### 🧱 Assembling Your Legos
You have total control. Enable or disable individual pieces in your config. The Kernel handles the wiring automatically.

```elisp
;; Example: Minimalist Setup (Code Only)
(setq dragonruby-enable-sprites nil)      ; Disable graphics
(setq dragonruby-enable-audio nil)        ; Disable audio
(setq dragonruby-enable-completion t)     ; Keep strict contract completion
(setq dragonruby-enable-paths t)          ; Keep fast navigation
```

| Lego Piece | Capability | Default |
|------------|------------|---------|
| `sprites` | Visual rendering, thumbnails | ✅ ON |
| `sprite-tools` | Image Editor, transformations | ✅ ON |
| `fonts` | TTF/OTF visuals, pangram viewer | ✅ ON |
| `audio` | Sound file detection, size info | ✅ ON |
| `colors` | Hex/RGB highlighting | ✅ ON |
| `concepts` | Educational concept popups | ✅ ON |
| `paths` | Project navigation, file jumping | ✅ ON |
| `completion` | Strict YAML API Contract | ✅ ON |
| `docs` | Documentation lookup interface | ✅ ON |

If you disable a parent (e.g., `sprites`), the Kernel **automatically** powers down its children (`sprite-tools`) to prevent errors. You don't have to manage dependencies. Just build your perfect editor.

---

### 📜 Level 5: The Contract (v0.1 Validated)
**"No magic. Just truth."**

An artisan autocompletion system that respects your definitions. It is **NOT** an LSP. It does not infer, guess, or think. It simply allows you to define a contract for your API, and the editor honors it.

- **Strict YAML Contract**: Define your API in `dragonruby_api.yml` using a strict, artisan-friendly schema (2-space indent, maps only).
- **Zero Hallucination**: It only completes what you explicitly define as a root (e.g. `args.`). It never guesses types.
- **Fail-Safe**: If your contract is invalid, the system silently steps aside. It never interrupts your flow with errors.
- **Introspection**: Use `M-x dragonruby-inspect-contract` to visualize the active knowledge tree.

**→ [📖 Complete Contract Guide](docs/CONTRACT.md)** (Schema, Examples, Philosophy)

### 🧠 Autocomplete Experience (New in v0.6.1)
- **Fluid Typing**: Type `arg` -> Select `args` -> System auto-inserts `.` -> Type `st` -> Select `state`.
- **Shortcuts**: Use `C-M-i` (Standard) or **`C-.`** (Windows-friendly) to trigger completion manually.
- **UI Recommendation**: For a "Google-like" popup experience, we highly recommend installing **`company-mode`**. The backend will automatically feed it data.
  - `M-x package-install RET company`
  - `(global-company-mode)`

---

## 📚 Documentation and Resources

- **[📖 IMAGE_EDITOR.md](docs/IMAGE_EDITOR.md)**: Complete image editor guide
- **[📖 FONTS.md](docs/FONTS.md)**: Font system and typography
- **[📖 CONTRACT.md](docs/CONTRACT.md)**: The Master Philosophy & Design Contract
- **[📜 MANIFESTO.md](docs/MANIFESTO.md)**: The Project's Existential Boundaries
- **[📖 INSTALLATION.md](docs/INSTALLATION.md)**: All installation methods
- **[🤝 CONTRIBUTING.md](docs/CONTRIBUTING.md)**: Guide for contributors
- **[💬 GitHub Issues](https://github.com/Macgyber/dragonruby-mode/issues)**: Report bugs or request features

---

*Forged for creators who refuse to settle. Forged for DragonRuby.*

**DragonRuby Emacs Mode — v0.7.0**

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
