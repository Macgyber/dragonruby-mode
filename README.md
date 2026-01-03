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

**This package uses a fault-tolerant micro-module architecture with a Multi-channel Real-time Engine.**

| Module | Responsibility | Status |
|--------|---------------|--------|
| `dragonruby-colors` | RGB/Hex color visualization | ✅ Stable |
| `dragonruby-sprites` | Image previews & navigation | ✅ Stable |
| `dragonruby-paths` | `require`/`load` navigation | ✅ Stable |
| `dragonruby-image-tools` | Image manipulation tools | 🟡 Beta |
| `dragonruby-docs` | Built-in Documentation System | 🚧 In Development |
| `dragonruby-concepts` | Keyword documentation links | 🚧 In Development |

### Key Architectural Features:
*   **Multi-channel Debounce**: Independent timers for Paths, Colors, and Sprites prevent collision and ensure instant reactivity while typing.
*   **Buffer-Local Root Caching**: Zero disk I/O for project detection after the first scan.
*   **LSP-Safe Integration**: Completion-at-point functions use `depth 100` and `:exclusive 'no'`, integrating seamlessly with Corfu, Company, or Ivy without hijacking your LSP.
*   **Standard-Compliant**: Uses native Emacs overlays and standard `match-data` protection for a crash-free experience.

**Users should only require `dragonruby-mode`**. Internal modules are managed automatically.

---

## What This Mode Does NOT Do

- ❌ **It is not an IDE** — no project management, no build systems
- ❌ **It does not replace LSP** — language intelligence belongs to LSP
- ❌ **It does not interfere with completion** — runs at depth 100, uses `:exclusive 'no`
- ❌ **It does not introduce popups** — uses overlays and minibuffer only

---

## ✅ Stable Features

### 🎨 Semantic Colors
Detects, visualizes, and **edits** real DragonRuby color values directly in your code.

```ruby
[255, 0, 0]            # Arrays (RGB)
[0, 255, 0, 128]       # Arrays (RGBA) with Transparency
0xFF00FF               # Hexadecimal
{ r: 255, g: 0, b: 0 } # Hashes
```

**Interaction (In Development)**: 
- A visual color box `■` appears next to the value.
- *Coming soon: Click to edit using a color picker.*

### 🖼️ Semantic Sprites
Visualizes your game assets immediately.

- **Inline Thumbnails**: Tiny preview next to the filename (scales with zoom)
- **Validation**: Cyan = Valid, Red = Missing, Orange = Unsupported
- **Click to Open**: Opens file in Emacs image viewer
- **Hover Tooltip**: Shows format and file size

### 🗺️ Smart Path Navigation
Turns your code into a hypertext web. **Real-time detection** marks valid paths with blue bold styling as you type.

| Shortcut | Action |
|----------|--------|
| `req` + `TAB` / `C-M-i` | Expands to `require ""` with cursor inside |
| `C-M-i` (in string) | Universal Autocomplete (Lists all `.rb`, `.png`, `.json`, etc.) |
| `C-c o` | **Global Jump:** Search and open any project file |
| `RET` / **Click** | Follow the "Link" to open the target file |

**Visual Feedback:**
- **Blue Bold**: Valid project path. Interactive.
- **Red Wave**: Broken link or missing file.

**Available Snippets:**
| Trigger | Expansion | Context |
|---------|-----------|---------|
| `req` | `require ""` | Ruby Code |
| `reqr` | `require_relative ""` | Ruby Code |
| `load` | `load ""` | Ruby Code |
| `read` | `$gtk.read_file ""` | Data Files |
| `json` | `$gtk.parse_json_file ""` | JSON Assets |
| `script` | `load_script ""` | DragonRuby Script |
| `spr` | `"sprites/.png"` | Assets |

> **Universal Autocomplete**: Unlike other tools, our autocompletion doesn't filter by context — if you're inside quotes, you can pick *any* file in your project, from code to sound effects.

### 🖼️ Image Tools
When viewing images in a DragonRuby project, a **Fluid & Adaptive Toolbar** appears:
- **Responsive Design**: UI automatically shrinks labels (`TRANSFORM` -> `T` -> 🧱) on small windows.
- **Timeline Navigation**: Versioned `Back (<)` and `Forward (>)` navigation with automatic snapshotting.
- **Accordion Logic**: Intelligent auto-collapse of menu groups to maximize space.
- **Debug Room (INF)**: Toggles a high-contrast background to reveal hidden transparent margins.
- **Magick Studio**: Professional tools (Trim, Compress, 2x Scaling, Grayscale, etc.) using `magick`.
- **Deep Jump**: Right-click `[Edit]` to quickly switch between external editors (Aseprite, Photoshop, etc.).

> Requires [ImageMagick](https://imagemagick.org/script/download.php#gsc.tab=0) installed.

---

## 🚧 In Development

### 🧠 Semantic Concepts
Keywords like `args`, `state`, `inputs`, and `outputs` are highlighted.
- Currently: Visual underline only
- Coming: Click to open documentation

> This feature requires `docs/concepts/` org files which are not yet complete.

---

## 🔮 Roadmap

- **learnDR-mode**: Educational mode with Org-mode integration 🚧 *In Development*
- **Hyper-Symbol Navigation**: `M-.` to jump to concept definitions 🚧 *In Development*

---

## Installation

Install using your preferred Emacs package manager (Straight, Doom, etc.).

Then enable the mode:

```elisp
(add-hook 'ruby-mode-hook #'dragonruby-maybe-enable)
(add-hook 'ruby-ts-mode-hook #'dragonruby-maybe-enable)
```

### Configuration

```elisp
;; Enable/disable specific features
(setq dragonruby-enable-colors t)       ; ✅ Stable
(setq dragonruby-enable-sprites t)      ; ✅ Stable
(setq dragonruby-enable-paths t)        ; ✅ Stable
(setq dragonruby-enable-image-tools t)  ; 🟡 Beta
(setq dragonruby-enable-docs nil)       ; 🚧 In Development
(setq dragonruby-enable-concepts nil)   ; 🚧 In Development
```

> For detailed manual installation steps per OS, see [docs/INSTALLATION.md](docs/INSTALLATION.md).

---

## Project Contract

Dragonruby-mode follows this contract:

1. **Structure is stable** — Core files won't be reshuffled without justification.
2. **Minor-mode first** — Always remains a buffer-local minor-mode.
3. **No UI creep** — Visuals must remain overlays. No panels, no popups.
4. **LSP is sacred** — Never replaces or interferes with LSP responsibilities.
5. **Non-Interference Rule** — All CAPFs use `:exclusive 'no` and depth 100.
6. **Visual Overlay Policy** — strictly follows the "Good Citizen" protocol (see [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md)).
7. **Semantic over features** — New features must add meaning, not noise.

> ⚠️ **Breaking this contract requires discussion, not impulse.**

---

## Documentation

- [Technical Architecture](docs/ARCHITECTURE.md)
- [Module Contract](docs/CONTRACT.md)
- [Keyboard Shortcuts](docs/SHORTCUTS.md)
- [Image Editor](docs/IMAGE_EDITOR.md)
- [Installation](docs/INSTALLATION.md)

---

*Built with ❤️ for the DragonRuby Community.*
