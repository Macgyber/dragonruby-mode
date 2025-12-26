# DragonRuby Emacs Mode 🐉

A semantic-first, Zero-UI development environment for DragonRuby Game Toolkit.

## 🌟 Philosophy

This mode transforms Emacs into a semantic editor for DragonRuby, utilizing **overlays** and **contextual intelligence** to verify "Does this asset exist?" or "What color is this?" without leaving the code.

## ✨ Features (Phase 1 Complete)

### 🎨 Semantic Colors
Automatically detects and visualizes colors in code. Supports virtually every DragonRuby syntax format:

*   **Arrays (RGB & RGBA)**: `[255, 0, 0]` and `[0, 255, 0, 128]`
*   **Hexadecimal**: `0xFF00FF` (Common in DR)
*   **Hashes (Hybrid Smart-Mode)**:
    *   **One-Liners**: `{ r: 255, g: 0, b: 0 }` -> Highlights the full block.
    *   **Multiline**: Resects indentation. Highlights fragments (`r: 255`) individually for a clean, professional look.
*   **Symbols (Data-Driven)**: `:red`, `:blue`, `:pico_orange`
    *   🚀 **Extensible Palettes**: Colors are loaded from `src/data/palettes.json`. You can add your own palettes (PICO-8, Nord, etc.) without touching Lisp code.

### 🖼️ Semantic Sprites
*   Hyperlink interaction for sprite paths.
*   **Visual Validation**:
    *   <u>Underlined</u>: File exists and is valid.
    *   <span style="color:red">Wavy Red</span>: File missing.
    *   <span style="color:orange">Wavy Orange</span>: Unsupported format (e.g. `.psd`).

### 🧭 Semantic Navigation
*   `require "app/game.rb"` become clickable links.
*   Auto-detects project root using `dragonruby` conventions.

## 🚀 Installation

### Quick Start (Dev Mode)
1.  Run the clean installer script:
    ```bash
    sh clean_install.sh
    ```
2.  Restart Emacs.

### Manual Installation
Add the `src` directory to your load-path and require the mode:

```elisp
(add-to-list 'load-path "/path/to/dragonruby-mode/src")
(add-to-list 'load-path "/path/to/dragonruby-mode/src/core")
(add-to-list 'load-path "/path/to/dragonruby-mode/src/features")
(require 'dragonruby-mode)
```

## 🛠️ Configuration

### Customizing Colors
Edit `src/data/palettes.json` to add new color symbols.
```json
{
  "my_theme": {
    "hero_color": "#FFCC00",
    "enemy_color": "#FF0000"
  }
}
```
Now using `:hero_color` in Ruby will show a preview!

## 📂 Project Structure
*   `src/core/`: Configuration and Project Root logic.
*   `src/features/`: Feature modules (Colors, Sprites, Paths).
*   `src/data/`: JSON data assets (Palettes).
*   `examples/`: Test suite (`main.rb`) to validate all features.
