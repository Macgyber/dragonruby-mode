# DragonRuby Emacs Mode 🐉

![DragonRuby Emacs Mode](docs/emacs-image.jpg)

> **Fault-Tolerant Micro-Module Architecture**
> Each feature is isolated and can be enabled/disabled without affecting others.

---

## Why This Exists

DragonRuby Emacs Mode exists to let developers **see meaning directly in code** — without turning Emacs into an IDE, without popups, without breaking flow.

It is built for people who believe the editor should amplify thought, not interrupt it.

---

## 🏗️ Architecture

**This package uses a strict Fault-Tolerant Micro-Module architecture with Centralized Core Infrastructure.**

| Module | Responsibility | Status |
|--------|---------------|--------|
| `dragonruby-core` | Central knowledge (Assets, Projects, Events, Utils) | 🧱 Solid |
| `dragonruby-colors` | RGB/Hex color visualization & Interactive Guards | ✅ Stable |
| `dragonruby-sprites` | Image previews & navigation (Dependency-free) | ✅ Stable |
| `dragonruby-paths` | Context-aware `require`/`load` navigation | ✅ Stable |
| `dragonruby-image-tools` | Image manipulation tools & Timeline Navigation | 🟡 Beta |
| `dragonruby-docs` | Built-in Knowledge System | 🚧 In Dev |
| `dragonruby-concepts` | Keyword documentation links | 🚧 In Dev |

### Key Architectural Features:
*   **Strict Modular Isolation**: Modules *never* depend on each other. Disabling "Sprites" won't break "Paths". All shared logic resides in `src/core/`.
*   **Infrastructure Facade**: `dragonruby-assets` acts as the single source of truth for file extensions and asset relationships.
*   **Multi-channel Debounce**: Independent timers for different scan types ensure zero lag and prevent collisions.
*   **Safety Guards**: Standardized interactive warnings for experimental or disabled features. No more silent failures.

---

## 🟢 Stable Features

### 🎨 Semantic Colors
Detects and visualizes real DragonRuby color values directly in your code.
- **Visual Swatches**: High-contrast icons that scale with your font.
- **Transparency Aware**: Proper blending for `[r,g,b,a]` and `{r:_, a:_}` values.
- **Interactive Guard**: Clicking a color swatch in stable mode provides a clear "In Development" notice for the picker.

### 🖼️ Semantic Sprites
Visualizes your game assets immediately without leaving the code.
- **Validation**: Cyan = Valid, Red = Missing, Orange = Unsupported.
- **Hybrid Source Finder**: Automatically detects if a `.png` has a source `.aseprite` in the directory or `art/` folder.
- **Deep Zoom**: Miniatures scale dynamically with Emacs text-zoom for total accessibility.

### 🗺️ Context-Aware Navigation (The Law)
Turns your code into a hypertext web. Marks valid paths with **blue bold** styling.

| Logic | Context Detection | Autocomplete Result |
|-------|-------------------|---------------------|
| `require` | Code requiring scripts | **Universal**: All files (app, lib, data, root) |
| `.sprites` | Sprite assignments | **Strict**: Only images in `sprites/` |
| `path:` | Hash property | **Strict**: Only images in `sprites/` |
| `read_file`| Data loading | **Strict**: Only files in `data/` |

**Snippets & Interaction (The "Double C-M-i" Workflow):**
- **Type `png` + `C-M-i`**: Expands to `".png"`.
- **Press `C-M-i` again**: **Instant List** of all `.png` files in `sprites/`.
- **Select**: Substitutes the filter with the full path: `"sprites/coin.png"`.
- `C-c o`: **Global Jump**. Search and open *any* project file.
- `RET` / **Click**: Follow the link or open the asset in the Emacs viewer.

---

## 🟡 Beta & Experimental (Developer Mode)

We believe in radical transparency. Unfinished features are available in the codebase for developers who want to help us test.

### 🧪 Unlocking the "Artist Portal"
Add these to your configuration to enable early-access tools:

```elisp
(setq dragonruby-enable-picker t)             ; Interactive Color Picker ■
(setq dragonruby-experimental-smart-jump t)   ; Jump to .aseprite from .png
(setq dragonruby-experimental-concepts-docs t); Deep Documentation System (Org)
```

### 🖼️ Adaptive Image Tools
When viewing images, a **Fluid Toolbar** appears:
- **Responsive Design**: UI labels shrink (`TRANSFORM` -> `T`) on small windows.
- **Timeline Navigation**: `Back (<)` and `Forward (>)` with automatic snapshotting.
- **Debug Stage (INF)**: High-contrast background to reveal transparent margins.

---

## 🔮 Roadmap (The "Ilusionamos" Section)

- **Hyper-Symbol Navigation**: `M-.` to jump to concept definitions 🚧
- **learnDR-mode**: Educational mode with interactive Org-mode tutorials 🚧
- **Real-time Pixel Sync**: Live updates between Aseprite and Emacs 🔮

---

## 🤝 Community & Feedback

We use **GitHub Issue Templates** to keep everything organized:
- **💡 Solicitud de Mejora**: Have an idea? Fill out our structured feature request.
- **🧪 Experimental Feedback**: Testing Dev Mode? Tell us what worked and what didn't.

---

---

## 📚 Docs & Standards

Explore the deep technical details and contribution guidelines:

| Documentation | Description |
|---------------|-------------|
| 🛠️ [Installation](docs/INSTALLATION.md) | OS-specific manual setup guide. |
| 🗺️ [Shortcuts](docs/SHORTCUTS.md) | Full cheatsheet for navigation and editing. |
| 🧱 [Architecture](docs/ARCHITECTURE.md) | Explaining the "Brick-Layer" core refactor. |
| 🛡️ [Project Contract](docs/CONTRACT.md) | Core rules (No UI Creep, LSP is Sacred). |
| 🎨 [Image Editor](docs/IMAGE_EDITOR.md) | In-depth guide to ImageMagick tools. |
| 🚧 [Contributing](docs/CONTRIBUTING.md) | How to help us test Developer Mode. |

---

*Built with ❤️ for the DragonRuby Community.*
