# DragonRuby Emacs Mode 🐉

A semantic-first, Zero-UI development environment for DragonRuby Game Toolkit.

[📖 Read the Technical Architecture](docs/ARCHITECTURE.md)

## 🌟 Philosophy

This mode transforms Emacs into a semantic editor for DragonRuby, utilizing **overlays** and **contextual intelligence** to verify "Does this asset exist?", "What color is this?", or "Where does this file lead?" without leaving the code.

## ✨ Features (Complete)

### 🎨 Phase 1: Semantic Colors
Automatically detects and visualizes colors in code using **Smart Hybrid Highlighting**.

*   **Arrays**: `[255, 0, 0]` and `[0, 255, 0, 128]`
*   **Hexadecimal**: `0xFF00FF`
*   **Hashes**:
    *   **One-Liners**: `{ r: 255, g: 0, b: 0 }` (Full Block)
    *   **Multiline**: `{ r: 255, ... }` (Fragmented, respects indentation)
*   **Symbols**: `:red`, `:dragon_green` (Data-Driven via `src/data/palettes.json`)
    *   🚀 **Pro Tip**: Add your own game colors to `palettes.json` for project-wide consistency!

### 🖼️ Phase 2: Semantic Sprites
Visualizes your game assets immediately.

*   **Inline Previews**: A tiny thumbnail (20px) appears right next to the filename.
*   **Rich Hover**: Hover over the path to see the full image + file size + dimensions.
*   **Validation**: 
    *   <span style="color:cyan">Cyan</span> = Valid Asset.
    *   <span style="color:red">Red</span> = Missing File.
*   **Autocomplete**: Type `"sprites/` and get a list of project images.

### 🗺️ Phase 3: Universal Navigation
Turns your code into a Hypertext web.

*   **Smart Requires**: Click `require 'app/player'` to jump to the file (handles implicit `.rb`).
*   **Data Links**: Strings like `"data/levels.json"` or `"docs/story.txt"` become clickable links if the file exists.
*   **Error Detection**: Requires pointing to non-existent files are marked in Red.

## 🛠️ Installation

1.  Clone this repository.
2.  Add to your `init.el`:
    ```elisp
    (add-to-list 'load-path "/path/to/dragonruby-mode/src")
    (require 'dragonruby-mode)
    ```
3.  Open a `.rb` file.

## 🛣️ Roadmap & Community Vote 🗳️

We need your feedback to prioritize the next wave of features:

*   **[ ] Phase 1.5: Interactive Color Picker**
    *   Click a color block to open the macOS/Windows native color picker.
*   **[ ] Phase 2.5: Sprite Optimizer**
    *   Right-click a sprite to Trim/Compress it (requires ImageMagick).
*   **[ ] Phase 3.1: Hyper-Symbol Navigation**
    *   Turn every method call (`enemy.attack`) into a clickable link to its definition.
*   **[ ] Phase 4: Hot Reload**
    *   Trigger DragonRuby reset from Emacs.

### 4. DragonRuby Wiki-Org (Visionary Phase) 🧠
*   **Contextual Glossary**: Press `F1` on any symbol (`args`, `outputs`) to open a dedicated **Side Panel**.
*   **Focused Learning**: Uses Emacs **Org-mode** to "narrow" the view. You see *only* the dictionary definition of that specific concept, distraction-free.
*   **Progress Tracking**: Mark concepts as `[X] Learned` directly in the docs.
*   **Multilingual**: Switch between `en.org`, `es.org`, `fr.org` instantly.

---
*Built with ❤️ for the DragonRuby Community.*
