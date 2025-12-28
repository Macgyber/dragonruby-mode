# DragonRuby Emacs Mode 🐉

![DragonRuby Emacs Mode](docs/emacs-image.jpg)

## Why This Exists

DragonRuby Emacs Mode exists to let developers **see meaning directly in code** — without turning Emacs into an IDE, without popups, without breaking flow.

It is built for people who believe the editor should amplify thought, not interrupt it.

## What This Is

A **semantic-first, overlay-only** minor mode for DragonRuby Game Toolkit development. It answers questions like "Does this asset exist?", "What color is this?", or "Where does this file lead?" — all without leaving your code.

## What This Mode Does NOT Do

- ❌ **It is not an IDE** — no project management, no build systems
- ❌ **It does not replace LSP** — language intelligence belongs to LSP
- ❌ **It does not introduce popups** — no side buffers, no modal UIs, no dashboards
- ❌ **It does not assume your project structure** — unless it detects DragonRuby explicitly

## Design Philosophy

Dragonruby-mode follows strict guarantees:

| Guarantee | Meaning |
|-----------|---------|
| 🧠 **Semantic-first** | The mode adds *meaning*, not syntax highlighting |
| 👻 **Overlay-only** | Text is never modified, replaced, or rewritten |
| 🧩 **Non-invasive** | Does not interfere with LSP, tree-sitter, or completion frameworks |
| 🪟 **Buffer-local** | Each buffer is isolated — no global state leakage |
| 🤝 **Coexistence** | The mode never "owns" the editor — it lives alongside other tools |

## Features

### 🎨 Semantic Colors
Detects and visualizes **real DragonRuby color values** directly in code.

```ruby
[255, 0, 0]            # Arrays (RGB)
[0, 255, 0, 128]       # Arrays (RGBA)
0xFF00FF               # Hexadecimal
{ r: 255, g: 0, b: 0 } # Hashes
```

### 🖼️ Semantic Sprites
Visualizes your game assets immediately.

- **Inline Previews**: A tiny thumbnail (20px) appears next to the filename
- **Rich Hover**: Hover over the path to see full image + file size + dimensions
- **Validation**: Cyan = Valid, Red = Missing
- **Navigation**: Click on a sprite path to jump to the file (or its source, e.g., `.aseprite`)

> 🛠️ **Optional**: An image editor is also available for basic manipulations. See [IMAGE_EDITOR.md](docs/IMAGE_EDITOR.md).

### 🗺️ Universal Navigation
Turns your code into a hypertext web.

- **Smart Requires**: Click `require 'app/player'` to jump to the file
- **Data Links**: Strings like `"data/levels.json"` become clickable links
- **Error Detection**: Requires pointing to non-existent files are marked in Red

## Installation

Install using your preferred Emacs package manager (Straight, Doom, etc.).

Then enable the mode:

```elisp
(add-hook 'ruby-mode-hook #'dragonruby-maybe-enable)
(add-hook 'ruby-ts-mode-hook #'dragonruby-maybe-enable)
```

> Use `C-h v user-init-file` or `M-:` to inspect your configuration if necessary.
>
> For detailed manual installation steps per OS, see [docs/INSTALLATION.md](docs/INSTALLATION.md).

## Project Contract

Dragonruby-mode follows this contract:

1. **Structure is stable**  
   Core files and responsibilities will not be reshuffled without strong justification.

2. **Minor-mode first**  
   The project will always remain a buffer-local minor-mode. No global hijacking.

3. **No UI creep**  
   Visuals must remain overlays. No panels, no popups, no dashboards.

4. **LSP is sacred ground**  
   Dragonruby-mode will never replace or interfere with LSP responsibilities.

5. **Semantic over features**  
   New features must add meaning, not noise.

6. **Documentation evolves with behavior**  
   Any behavioral change must be reflected in the README or docs.

> ⚠️ **Breaking this contract requires discussion, not impulse.**

---

## Roadmap

- [ ] **Phase 3.1**: Hyper-Symbol Navigation
- [ ] **learnDR-mode**: Educational mode with Org-mode integration (In Development)

## Documentation

- [Technical Architecture](docs/ARCHITECTURE.md)
- [Keyboard Shortcuts](docs/SHORTCUTS.md)
- [Contributing](docs/CONTRIBUTING.md)

*Built with ❤️ for the DragonRuby Community.*
