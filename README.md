# DragonRuby Mode for Emacs

**Semantic Extension for DragonRuby.**

> "The goal is not speed of typing, but clarity of thought."

This mode enhances Emacs with semantic understanding of DragonRuby projects, focusing on visual clarity and navigation without adding UI clutter.

## Core Features

### 1. 🎨 Semantic Color Interaction
- **Detection**: Automatically highlights colors in two formats:
  - **Arrays**: `[255, 0, 0]` or `[0xFF, 0, 0]` (Decimal & Hex).
  - **Hashes**: `r: 255, g: 0, b: 0` (Supports any order, e.g., `g: 0, r: 255...`).
- **Interaction**: **Click** on any color text to edit its values inline (no popups).

### 2. 👾 Asset Intelligence
- **Sprite Preview**: Hover over `"sprites/player.png"` to see:
  - **Thumbnail**: Actual image preview.
  - **Metadata**: `📏 WxH px (FORMAT) SIZEkb` (e.g., `32x32 PNG 12kb`).
- **Optimization**: Instant tooltip feedback (0.1s delay).
- **Validation**:
  - **Cyan Underline**: File exists.
  - **Red Wavy Line**: File missing.
  - **Orange Underline**: Unsupported format.
- **Support**: `png`, `jpg`, `bmp`, `gif`.
- **Navigation**: Click on any asset path to open the file.

### 3. 🔗 Project Navigation
- **Clickable Requires**: `require "app/logic.rb"` becomes a clickable link (Cyan=Exists, Red=Missing).
- **Contextual Autocomplete**: Smart completion for `app/`, `sprites/`, and `audio/` when typing inside strings.

## Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/Macgyber/dragonruby-mode.git ~/.emacs.d/dragonruby-mode
   ```
2. Add to your `init.el`:
   ```elisp
   (add-to-list 'load-path "~/.emacs.d/dragonruby-mode/src")
   (require 'dragonruby)
   ```

## Usage

The mode activates **automatically** when you open a Ruby file containing `def tick`. You will see `DR` in the modeline.

## Configuration

Customize via `M-x customize-group RET dragonruby RET`.

| Variable | Default | Description |
|----------|---------|-------------|
| `dragonruby-enable-color-preview` | `t` | Show real color overlays. |
| `dragonruby-enable-sprite-preview` | `t` | Show image thumbnails on hover. |
| `dragonruby-enable-require-linking` | `t` | Make `require` paths clickable. |
| `dragonruby-max-overlays-per-type` | `50` | Performance limit for large files. |

## Architecture

See [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) for technical details on how the semantic engine works.

## Development

To verify that your changes compile correctly, you can use the included `Makefile`:

```bash
make compile
```

## Development

To verify that your changes compile correctly, you can use the included `Makefile`:

```bash
make compile
```

This will byte-compile all source files, identifying syntax errors or missing requirements immediately.

## Roadmap / Potential Features
We are collecting feedback on the following ideas:
- **Visual Color Picker**: A GUI popup to select colors instead of typing numbers. (Pending community interest: strict "no-popup" philosophy vs usability).
