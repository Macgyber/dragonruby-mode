# DragonRuby Mode Architecture

## Core Principles

1. **Semantic Overlays**: Information is painted on top of the buffer (colors, images, links) without altering the text.
2. **Zero-UI**: Information appears *in-place*. No sidebars, no popups.
   - **Colors**: Inline backgrounds
   - **Sprites**: Inline thumbnails + Hover tooltips
   - **Paths**: Clickable hyperlinks
3. **Project-Aware**: All lookups are resolved relative to the DragonRuby project root.
4. **Buffer-Local**: Each buffer is isolated — no global state leakage.

## Module Breakdown

### [`dragonruby-mode.el`](file:///e:/ANTIGRAVITY/dragonruby-emacs/packages/dragonruby-mode/dragonruby-mode.el) (Entry Point)
- Defines the minor mode
- Coordinates module loading
- Provides activation/deactivation hooks

### [`src/dragonruby-core.el`](file:///e:/ANTIGRAVITY/dragonruby-emacs/packages/dragonruby-mode/src/dragonruby-core.el) (Project Utilities)
- Finds project root (dominating file `app/main.rb` or `.dragonruby/`)
- **Smart Source Finding**: Resolves source files (`.aseprite`, `.graphite`) in local or `art/` directories.
- Provides shared utilities and configuration for all modules.

### [`src/dragonruby-colors.el`](file:///e:/ANTIGRAVITY/dragonruby-emacs/packages/dragonruby-mode/src/dragonruby-colors.el) (The Painter)
- **Scanning**: Hybrid approach
  - One-liners → Full block painting (contiguous RGB/RGBA)
  - Multiline → Fragment painting (respects indentation)
- **Data Source**: Reads palettes from `src/data/palettes.json`

### [`src/dragonruby-sprites.el`](file:///e:/ANTIGRAVITY/dragonruby-emacs/packages/dragonruby-mode/src/dragonruby-sprites.el) (The Asset Manager)
- **Dual Visualization**:
  1. Inline: 20px thumbnail via `after-string` overlay
  2. Hover: 300px image via `help-echo`
- **Autocomplete (CAPF)**: Scans `.png/.jpg` files in project
- **Validation**: Cyan = Valid, Red = Missing, Orange = Unsupported
- **Smart Jump**: Opens source files instead of sprites if available (Experimental).

### [`src/dragonruby-image-tools.el`](file:///e:/ANTIGRAVITY/dragonruby-emacs/packages/dragonruby-mode/src/dragonruby-image-tools.el) (The Editor)
- **Header Toolbar**: Appears when viewing images
  - View controls: zoom, rotate, reset, info
  - ImageMagick operations: trim, compress, resize, flip, effects
  - **External Edit**: Delegates to Aseprite/Graphite/System Default via `dragonruby-core` logic.

### [`src/dragonruby-paths.el`](file:///e:/ANTIGRAVITY/dragonruby-emacs/packages/dragonruby-mode/src/dragonruby-paths.el) (The Navigator)
- **Universal Linker**: Scans for Ruby `require` and data files
- **Interaction**: Creates clickable hyperlinks to open files

## Execution Flow

```
User types → after-change-functions (debounced 0.3s)
          → Scanners run (colors, sprites, paths)
          → Paths resolved to absolute
          → Overlays created/updated
          → User interaction (click/hover)
```

## Data Files

| File | Purpose |
|------|---------|
| `src/data/palettes.json` | Named color definitions for symbols like `:red` |

## Extensibility

- **Colors**: Add entries to `src/data/palettes.json`
- **Sprites**: Add extensions to `dragonruby-sprite-source-extensions` (e.g. `.kra`)
