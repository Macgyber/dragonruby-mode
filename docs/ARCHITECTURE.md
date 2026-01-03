# DragonRuby Mode Architecture

## Core Principles

1. **Semantic Overlays**: Information is painted on top of the buffer (colors, images, links) without altering the text.
2. **Zero-UI**: Information appears *in-place*. No sidebars, no popups.
   - **Colors**: Inline backgrounds
   - **Sprites**: Inline thumbnails + Hover tooltips
   - **Paths**: Clickable hyperlinks
3. **Project-Aware**: All lookups are resolved relative to the DragonRuby project root.
4. **Buffer-Local**: Each buffer is isolated — no global state leakage.
5. **Keyboard-First**: All features accessible via keyboard (`RET`, `C-c RET`), mouse is optional.
6. **Visual Overlay Policy**: A non-invasive approach to semantic visualization.

## 🧘 Visual Overlay Policy (The "Good Citizen" Protocol)

DragonRuby Mode follows a strict "Observe and Paint" philosophy. It acts as a fine varnish over your code, never a reconstruction.

### 🧠 1. Reactivity (Hooks)
No polling or aggressive loops. We use native Emacs reactivity:
- `after-change-functions`: Reacts only when you type or delete.
- `window-configuration-change-hook`: Ensures visibility when switching buffers.

### 🎨 2. Dynamic Painting (Overlays)
Visuals are NOT text. They use Emacs **Overlays**:
- **Non-destructive**: They live "above" the text. Deleting the overlay never deletes your code.
- **Dynamic**: Created on-the-fly and destroyed immediately when no longer needed.
- **Immediate Invalidation Rule**: Any text change within an overlay's range MUST invalidate that overlay immediately to avoid "visual ghosts" or stale information.
- **LSP-Safe**: Overlays do not interfere with buffer contents, so LSP, `company`, and `corfu` see raw, clean code.

### ⏳ 3. Rhythm (Debounce)
We don't scan on every keystroke. We wait for the user to "breathe":
- **Multi-channel Debounce**: Independent timers for Colors (0.25s), Sprites (0.15s), and Paths (0.05s).
- **Efficiency**: Avoids lag and visual flickering.

### 🤝 4. Coexistence (Priority)
We never "win" visual wars. 
- **Low Priority**: All overlays use `priority -50`.
- **Logic**: If another plugin (like `rainbow-mode` or `lsp-semantic-tokens`) wants to paint a zone, they win. Our visuals stay underneath or yield.

### 🚫 5. Non-Interference
- ❌ No redefinition of `font-lock-keywords`.
- ❌ No modification of `syntax-table`.
- ❌ No "stealing" of global keybindings.
- ❌ No modifications to the buffer string (`inhibit-modification-hooks` is respected).

## System Architecture (v0.5.5 Refactor)

The system is now driven by a **Strict Modular Isolation** model. Logic for discovering files and assets has been moved from individual modules to the **Core Infrastructure**.

```text
src/
├── core/                    ;; CORE INFRASTRUCTURE (Brick Layer)
│   ├── dragonruby-project.el   ;; Root finding & caching
│   ├── dragonruby-assets.el    ;; THE ENGINE: Extension knowledge & File collection
│   ├── dragonruby-utils.el     ;; OS-level helpers, Debounce, & Safety Warnings
│   ├── dragonruby-events.el    ;; Lightweight event bus
│   └── dragonruby-registry.el  ;; Concept Census
├── colors/                  ;; COLOR SUBSYSTEM (Consumer)
├── image-tools/             ;; EDITOR SUBSYSTEM (Consumer)
├── paths/                   ;; NAVIGATION SUBSYSTEM (Consumer)
├── sprites/                 ;; SPRITE SUBSYSTEM (Consumer)
├── dragonruby-mode.el       ;; Entry Point & Activation Guardian
└── ... facades ...
```

## Module Breakdown

### 1. Core (`src/core/`) - The Single Source of Truth
**Responsibility**: Foundation and shared intelligence.
- **Projects**: Detects `app/main.rb`. Transparent caching prevents disk thrashing.
- **Assets**: **The Engine**. Centralizes *all* file extensions (`.png`, `.rb`, `.json`). All modules ask this module for project files.
- **Utils**: Standardized "In Development" warning system.
- **Isolation Rule**: Modules (Paths, Sprites, Colors) *NEVER* depend on each other. They only depend on the Core.

### 2. Colors (`src/colors/`)
**Responsibility**: Visualization & Editing.
- **Interaction**: If the interactive picker is disabled, it triggers a Core safety warning.

### 3. Paths (`src/paths/`)
**Responsibility**: Hypertext Navigation.
- **Context-Awareness**: Detects if your cursor is near `.sprites`, `.labels`, or `require` and asks the Core Assets engine for the specific file types allowed in that context.

### 4. Sprites (`src/sprites/`)
**Responsibility**: Visual Anchors.
- **Model**: Pure metadata. Now lightweight as file discovery is handled by the Core.
- **Overlay**: Inline thumbnails and source-jumping logic.

### 5. Image Tools (`src/image-tools/`)
**Responsibility**: Content Modification.
- Provides a toolbar when viewing images inside Emacs.
- Wraps ImageMagick CLI to perform non-destructive edits (Trim, Crop, Flip).

### 6. Concepts (`src/concepts/`) - In Development
**Responsibility**: Knowledge Connection.
- Detects keywords like `args`, `outputs`, `tick`.
- Applies subtle interactive underlines for documentation lookups.

## Execution Flow (Multi-channel Real-time)

```
User types → after-change-functions
           → Channel: Paths   (0.05s) → Resolve & Linkify
           → Channel: Sprites (0.15s) → Create Thumbnails
           → Channel: Colors  (0.30s) → UI Swatches
           
           * All channels run in parallel tasks protected by save-match-data. *
           → User interaction (RET/click/hover)
```

## Keyboard Navigation

All overlays support:
- `RET` - Activate (open file, edit color)
- `C-c RET` - Alternative activation
- `mouse-1` - Click activation

## Extensibility

- **Colors**: Modify `dragonruby-colors.el` to add named colors
- **Sprites**: Add extensions to `dragonruby-sprite-source-extensions`
- **Paths**: Add extensions to `dragonruby-data-extensions`

## 🚀 Snippet & Completion Registry (v0.5.7)

The completion system is now driven by a centralized **Snippet Registry** (`dragonruby-registry.el`). 

- **Modular Registration**: Each module (Sprites, Audio, Paths) registers its own snippets during initialization.
- **Context-Aware Completion**: `dragonruby-smart-complete` acts as a high-level router:
  1. It first attempts to expand a snippet at the cursor.
  2. If already inside a string, it detects the **Context Type** (`'ruby`, `'sprite`, `'data'`).
  3. It queries the Core Assets engine for valid files and provides an **Instant List** in the minibuffer.
- **The Law of Formats**: Candidates are formatted as `.ext | path` to ensure immediate visual identification of asset types.
- **Extensible**: Users can register their own snippets by calling `dragonruby-registry-register` under the ID `'user-snippets`.
