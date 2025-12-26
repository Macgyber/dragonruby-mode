# DragonRuby Mode Architecture

## 🏛️ Core Principles

1.  **Semantic Overlays**: We do not use font-lock (regex highlighting) for semantic features. We use `overlays`, which allow for rich interaction (click, hover), boxes, and arbitrary styling independent of syntax highlighting.
2.  **Zero-UI**: Information appears *in-place*. No sidebars or popups unless explicitly requested by interaction (e.g. Color Picker).
3.  **Data-Driven**: Hardcoded lists are avoided. Critical data (like color palettes) lives in external JSON files for extensibility.

## 🧩 Module Breakdown

### 1. `dragonruby-mode.el` (Orchestrator)
*   Manages the minor mode state.
*   Enables/Disables sub-features (`colors`, `sprites`, `paths`).
*   Hooks into `after-change-functions` to trigger incremental scanning.

### 2. `features/dragonruby-colors.el` (The Painter)
*   **Scanning Strategy**: "Context Window". For complex structures like Hashes, we find an anchor (`r:`) and scan a limited forward window (200 chars) for related keys (`g:`, `b:`).
*   **Contrast Logic**: Automatically calculates luminance to set text color (black/white) for readability.
*   **Data Source**: Reads `src/data/palettes.json` at startup via `json-read-file`. Flattens nested JSON categories into a single `O(1)` Hash Table lookup for symbol resolution.

### 3. `features/dragonruby-sprites.el` (The asset Manager)
*   Scans string literals ending in image extensions.
*   **Validation**: Checks `file-exists-p` relative to the Project Root.
*   **Feedback**: Uses distinct overlay styles (color/underline style) to communicate validity instantly.

### 4. `core/dragonruby-project.el` (The Brain)
*   Locates the `app/main.rb` or `.dragonruby/` marker to establish root.
*   Resolves relative paths (`sprites/foo.png`) to absolute system paths.

## 🔄 Execution Flow

1.  **User types** in buffer.
2.  `after-change-functions` hook fires.
3.  Each enabled feature (`scan-colors`, `scan-sprites`) runs incrementally.
4.  **Colors**:
    *   Regex search finds candidate.
    *   Validation logic runs (e.g. "Do we have r, g, and b?").
    *   `dragonruby--make-overlay` creates the visual block.
5.  **Data Persistence**: Overlays persist until explicitly cleared or the buffer text changes significantly.

## 🎨 Extensibility
New color palettes can be added by modifying `src/data/palettes.json`. Emacs loads this map into memory once, ensuring high performance during typing.
