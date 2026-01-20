# DragonRuby Emacs Mode 🐉

<div align="center">
  <img src="knowledge/emacs-image.jpg" width="50%" alt="DragonRuby Emacs Mode">
  <br><br>
  <b>"It wasn't supposed to be possible."</b><br>
  We built this not just to write code, but to feel it. A love letter to the DragonRuby community and the infinite potential of Emacs.
</div>

---

## The New Standard
🏛️ **The New Standard: A Professional Baseline**

For years, we accepted that "powerful" meant "complicated" and "fast" meant "ugly". We didn't agree.

**DragonRuby Emacs Mode** is a milestone. It is the result of obsessively asking: *"What if your editor felt less like a tool and more like an extension of your mind?"*

We haven't just added features; we have established an **Optimized Engine**. From the zero-latency sprite rendering to the industrial-grade memory management, every line of code here exists to protect your flow. We don't want to receive the credit; we want you to create the impossible, and forget we are even here.

This is our contribution to the craft. This is the new baseline.
## 📑 Table of Contents

- [The New Standard](#the-new-standard)
- [The Stable Features](#the-stable-features)
  - [Level 1: Color Intelligence](#level-1-color-intelligence)
  - [Level 2: Sprite Management](#level-2-sprite-management)
  - [Asset Workshop: Integrated Image Editor](#asset-workshop-integrated-image-editor)
  - [Level 4: The Creative Hub](#level-4-the-creative-hub)
  - [Level 3: Intelligent Navigation](#level-3-intelligent-navigation)
- [Tactical Features](#tactical-features)
  - [Audio Assistant](#audio-assistant)
  - [Font Visualizer](#font-visualizer)
  - [Level 5: Time Travel Module (Stargate)](#level-5-time-travel-module)
- [Installation](#installation)
- [Architecture: The Lego System](#architecture-the-lego-system)
  - [The Kernel](#the-kernel)
  - [The Autonomous Kernel](#the-autonomous-kernel)
- [Documentation and Resources](#documentation-and-resources)

---

## The Stable Features
🛠️ **The Stable Features (The Foundational Base)**

### Level 1: Color Intelligence
🎨 **Level 1: Color Intelligence (Colors)**
Visualize the invisible. Your game's palette, alive in your code.
- **Living Color Blocks**: Hex literals (`0xFF00FF`), RGB/RGBA Arrays `[255, 0, 0]`, and Hashes `{r: 255, g: 0, b:0}` — the moment you type them, they transform into visual beacons revealing their true color. Multi-line hashes handled with surgical precision.
- **Contrast Intelligence**: Text automatically adapts (black or white) to ensure readability on any background.

<div align="center">
  <img src="knowledge/colors-preview.png" alt="Color Detection Preview" width="60%">
  <br><em>Arrays, RGBA hashes, and hexadecimal — all detected and visualized.</em>
</div>

### Level 2: Sprite Management
🖼️ **Level 2: Sprite Management (Sprites)**
Stop coding blind. See your world as you build it.
- **Inline Intelligence**: High-definition thumbnails embedded directly in your code.
- **The Validation Standard**: Cyan = Ready, Red = Missing, Orange = Illegal Format.
- **"Double C-M-i" Workflow** (CAPF):
  - Type `spr` + `C-M-i` → Expands to `""` (cursor inside).
  - Press `C-M-i` again → You'll see **only valid sprites** from your project.
  - **Without plugin**: You'd see all project files mixed together.
  - **With plugin**: You see only files from the `sprites/` folder with valid extensions (PNG, BMP, JPG, etc.).

> 💡 **Immersive UI**: By default, suggestions appear in the minibuffer. For a **floating popup** experience like modern IDEs, install **`company-mode`**. The plugin automatically feeds it your sprites, fonts, and paths.
> ```elisp
> M-x package-install RET company
> (global-company-mode)
> ```

### Asset Workshop: Integrated Image Editor
🎨 **Asset Workshop: Integrated Image Editor**

Press `RET` or `C-c C-o` on any sprite in your code and the header-line transforms into a **complete workstation**. You'll have access to a professional toolset with non-destructive timeline, tool groups (VIEW, TRANSFORM, COLOR, SYSTEM, CREATIVE), and over 15 operations: remove backgrounds, trim margins, pixel-perfect scaling (2x/0.5x), apply effects, rotate, invert, compress, and much more. All without leaving Emacs.

<div align="center">
  <img src="knowledge/forge-toolbar.png" alt="The Forge Toolbar" width="80%">
  <br><em>VIEW, TRANSFORM, COLOR, SYSTEM, CREATIVE — Five tool groups, infinite power.</em>
</div>

### Level 4: The Creative Hub
🌐 **Level 4: The Creative Hub (Beta)**
Your bridge to the ecosystem. Located within the Image Editor header-line:
- **Instant Web Tools**: One-click access to **Graphite**, **Piskel**, **Lospec**, **Itch.io**, and the DragonRuby Discord.
- **Customizable**: Add your own tools (Web URLs or Local Executables) via the `[+]` button.
- **Adaptive**: Tools have custom colors and can be hidden/shown to fit your workflow.

**→ [📖 Complete Image Editor Guide](knowledge/IMAGE_EDITOR.md)** (Timeline, Tool groups, Creative Hub)


### Level 3: Intelligent Navigation
⚡ **Level 3: Intelligent Navigation (The Standard)**
Eliminate journey friction. Move at the speed of thought.
- **Contextual Intelligence**: The plugin knows when you're in a `require`, a `read_file`, or a `path:` assignment and filters your destinations accordingly.
- **Dimensional Links**: Every path is a portal. `C-c C-o` or `RET` to jump directly to any file or asset.
- **CAPF Completion**: Press `C-M-i` inside a string to see files of the correct type based on context. With **`company-mode`** installed, suggestions appear in a floating IDE-style popup.

---

## Tactical Features
💎 **Tactical Features (Beta)**

### Audio Assistant
🔊 **Audio Assistant (Audio) — Beta**
Audio components are no longer silent text. Feel your game's heartbeat.
- **Active Indicator**: Valid assets are highlighted for easy identification.
- **Format Verification**: Instant notification of non-optimized formats.
- **File Metadata**: Instant file size information on hover.

### Font Visualizer
🅰️ **Font Visualizer (Fonts)**

> ⚠️ **Requires [ImageMagick](https://imagemagick.org/)** to render previews.

Did you pick the right font? Why doesn't it look like you expected? Stop guessing. Press `RET` on any font and visualize it **in real-time** with your own text, pangrams in 5 languages, or the full alphabet.

- **Smart Thumbnails**: Automatically adapt to your theme (dark/light).
- **Context Preview**: The plugin reads nearby `text:` values (limited to 11 characters to prevent overflow). Write `text: "Press Start"` near your font, and the thumbnail shows that phrase.

<div align="center">
  <img src="knowledge/font-context-preview.png" alt="Font Context Preview" width="60%">
  <br><em>The plugin reads "Press Start" from nearby text: and renders it in the font.</em>
</div>

- **Concept Exploration**: Explore game concepts and patterns.
- **Stargate (Time-Travel)**: Record, rewind, and branch your game's timeline (experimental). [Read more](knowledge/STARGATE.md).
- **Creative Hub**: Explore fonts from within the editor. Available buttons: **Google Fonts**, **DaFont**, **Font Squirrel**. Add `[+]` or remove `[-]` your own links to customize.

**→ [📖 Complete Font System Guide](knowledge/FONTS.md)** (ABC/Pangram Viewer, Typography resources, Troubleshooting)

### Level 5: Time Travel Module
🌌 **Level 5: Stargate Module (Time-Travel) — v1.0 Stable**
Master the simulation. Record, Mutate, and Rewind with atomic precision.

- **Session Recorder**: Continuous recording of states and seeds into a JSON-backed history.
- **Code Injector**: Real-time code injection with reliable idempotency and sandbox-compliant loading.
- **IPC Filtering**: Advanced filtering that keeps Emacs fluid even during massive engine log bursts.
- **Timeline Navigation**: Visual scrubber to jump between different states with hierarchical inheritance.

**→ [📖 Complete Stargate Guide](knowledge/STARGATE.md)** (State tracking, Atomic Injections, Timeline Navigation)


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

### 🏭 The Industrial Standard
**"The visible speed is born from the invisible order."**

We have re-engineered the core to meet **SRE & Performance Engineering** standards:
- **Zero Blocking**: Sprite scanning is now instantaneous. Heavy image processing is strictly deferred to idle time (lazy loading).
- **Controlled Memory**: Implemented **Singleton Timer Patterns** to prevent resource leaks during rapid interaction.
- **Silent Core**: The system now handles invalid contexts (like scratchpads) with absolute silence, ensuring stability 100% of the time.

---

## Installation
📦 **Installation**

Choose your preferred method. For a quick setup in **Doom Emacs**, simply copy these blocks:

```elisp
;; 1. In ~/.doom.d/packages.el
(package! dragonruby-mode :recipe (:host github :repo "Macgyber/dragonruby-mode"))

;; 2. In ~/.doom.d/config.el
(use-package! dragonruby-mode
  :hook ((ruby-mode . dragonruby-maybe-enable)
         (ruby-ts-mode . dragonruby-maybe-enable)))
```

**→ [📖 Complete Installation Guide](knowledge/INSTALLATION.md)** (Detailed instructions for Doom Emacs, straight.el, Spacemacs, use-package, manual, and more)

---

## Architecture: The Lego System
🏗️ **Architecture: The Lego System (v1.0.0)**
"Complexity is the enemy. Modularity is the shield."

We have re-forged the core into a **Lego Architecture**. The system is no longer a monolith; it is a collection of independent, intelligent pieces orchestrated by a central Kernel.

### The Kernel
🧠 **The Kernel**
The Kernel is the central nervous system. It strictly enforces the **Three Base Policies**:
1.  **Namespace Policy**: Every module owns its space. No collisions.
2.  **Capability Policy**: Modules request features (e.g. `:rendering`), and the Kernel finds the provider.
3.  **Initialization Policy**: Nothing runs until you ask for it. Zero background overhead.

### 🧱 Assembling Your Legos
You have total control. You can enable or disable individual pieces in your configuration, or handle it visually:

- **Visual Method**: Run `M-x customize-group RET dragonruby` to see all available modules and toggle them with a single click.
- **Config Method**: Add these lines to your `config.el` (Doom) or `init.el` (Vanilla) **before** loading the mode:

```elisp
;; Example: Minimalist Setup (Code Only)
(setq dragonruby-enable-sprites nil)      ; Disable graphics
(setq dragonruby-enable-audio nil)        ; Disable audio
(setq dragonruby-enable-completion t)     ; Keep schema-based autocomplete
(setq dragonruby-enable-paths t)          ; Keep fast navigation
```

| Lego Piece | Capability | Default |
|------------|------------|---------|
| `sprites` | Visual rendering, thumbnails | 🔴 OFF |
| `sprite-tools` | Image Editor, transformations | 🔴 OFF |
| `fonts` | TTF/OTF visuals, pangram viewer | 🔴 OFF |
| `audio` | Sound file detection, size info | 🔴 OFF |
| `colors` | Hex/RGB highlighting | 🔴 OFF |
| `concepts` | Educational concept popups | 🔴 OFF |
| `paths` | Project navigation, file jumping | 🔴 OFF |
| `stargate` | Time travel, branching, replay | 🔴 OFF |
| `completion` | Strict YAML API Schema | ✅ ON |
| `guide` | Knowledge guidance system (Sidebar) | 🔴 OFF |

If you disable a parent (e.g., `sprites`), the Kernel **automatically** powers down its children (`sprite-tools`) to prevent errors. You don't have to manage dependencies. Just build your perfect editor.

### The Autonomous Kernel
🧠 **The Autonomous Kernel (v1.0.0)**
The v1.0.0 update introduces the **Autonomous Kernel**, a central authority inspired by micro-kernel operating systems.
- **The System Ledger**: Every timer, hook, and process must be registered. If the Kernel doesn't know it, it shouldn't exist.
- **Project Root Anchor**: Project root is detected once and locked. No more background disk crawling.
- **Lazy Implementation**: Heavy logic is split from manifests. Functions and modes are only loaded from disk when a module is actually used.
- **Performance Excellence**: Idle CPU usage reduced from 97% to <2% through lazy visual asset processing.
- **Standard Hot-Reload (F6)**: A reliable reload following a strict `Shutdown -> Unload -> Load -> Reboot` cycle.
- **Process Sentinel**: Background safety net that scans the Emacs subsystem for orphaned processes or timers.

- **The Knowledge Base**: Loads definitions from `.org` files into a unified hash-table cache.
- **Context Intelligence**: Scans your code and queries the Knowledge Kernel for instant context.
- **Tooltip Management**: Tooltips are suppressed when the Guidance Sidebar is open to keep your workspace clear.
- **Follow Mode**: The sidebar automatically synchronizes with the concept under your cursor.

---

### 📜 API Schema (Validated)
**"No magic. Just truth."**

A precise autocompletion system that respects your definitions. It is **NOT** an LSP. It does not infer, guess, or think. It simply allows you to define a schema for your API, and the editor honors it.

- **Strict YAML Schema**: Define your API in `dragonruby_api.yml` using a strict, human-friendly format (2-space indent, maps only).
- **Deterministic Completion**: It only completes what you explicitly define as a root (e.g. `args.`). It never guesses types.
- **Fail-Safe**: If your schema is invalid, the system silently steps aside. It never interrupts your flow with errors.
- **Introspection**: Use `M-x dragonruby-inspect-contract` to visualize the active knowledge tree.
- **Automatic Detection**: The system looks for `dragonruby_api.yml` in your project root. If missing, it automatically uses the global schema residing in the plugin's own folder.


### 🧠 Autocomplete Experience
- **Fluid Typing**: Type `arg` -> Select `args` -> System auto-inserts `.` -> Type `st` -> Select `state`.
- **Smart Dot**: Typing a `.` after a valid contract keyword (like `args`) triggers the completion menu **automatically**.
- **Shortcuts**: Use `C-M-i` (Standard) or **`C-.`** (Windows-friendly) to trigger completion manually.
- **UI Recommendation**: For a "Google-like" popup experience, we highly recommend installing **`company-mode`**. The backend will automatically feed it data.
  - `M-x package-install RET company`
  - `(global-company-mode)`

---

## Documentation and Resources
📚 **Documentation and Resources**

- **[📖 IMAGE_EDITOR.md](knowledge/IMAGE_EDITOR.md)**: Complete image editor guide
- **[📖 FONTS.md](knowledge/FONTS.md)**: Font system and typography
- **[📖 INSTALLATION.md](knowledge/INSTALLATION.md)**: All installation methods
- **[🤝 CONTRIBUTING.md](knowledge/CONTRIBUTING.md)**: Guide for contributors
- **[💬 GitHub Issues](https://github.com/Macgyber/dragonruby-mode/issues)**: Report bugs or request features

---

*Forged for creators who refuse to settle. Forged for DragonRuby.*

**DragonRuby Emacs Mode — v1.0.0 (Stable)**

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![CI](https://github.com/Macgyber/dragonruby-mode/actions/workflows/ci.yml/badge.svg)](https://github.com/Macgyber/dragonruby-mode/actions/workflows/ci.yml)
