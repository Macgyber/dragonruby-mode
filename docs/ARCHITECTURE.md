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

## System Architecture

```text
src/
├── core/                    ;; CORE INFRASTRUCTURE
│   ├── dragonruby-project.el   ;; Root finding
│   ├── dragonruby-utils.el     ;; Helpers (debounce)
│   ├── dragonruby-assets.el    ;; Asset rules
│   └── dragonruby-events.el    ;; Event bus
├── colors/                  ;; COLOR SUBSYSTEM
│   ├── dragonruby-color-scanner.el ;; Regex logic (Hex/RGB/Hash/Alpha)
│   ├── dragonruby-color-visuals.el ;; Overlays + Box Rendering
│   ├── dragonruby-color-picker.el  ;; Interactive Edit Logic
│   └── dragonruby-color-utils.el   ;; Math
├── concepts/                ;; CONCEPT DOCUMENTATION (In Development)
│   └── dragonruby-concept-visuals.el
├── image-tools/             ;; EDITOR SUBSYSTEM
│   ├── dragonruby-image-modify.el  ;; ImageMagick wrappers
│   ├── dragonruby-image-view.el    ;; UI Controls
│   └── dragonruby-image-ui.el      ;; Header/Buttons
├── paths/                   ;; NAVIGATION SUBSYSTEM
│   ├── dragonruby-path-model.el    ;; Data (extensions, snippets)
│   ├── dragonruby-path-fs.el       ;; File System
│   ├── dragonruby-path-snippets.el ;; Snippet expansion
│   ├── dragonruby-path-overlay.el  ;; Overlays
│   └── dragonruby-path-actions.el  ;; Interactive commands
├── sprites/                 ;; SPRITE SUBSYSTEM
│   ├── dragonruby-sprite-model.el
│   ├── dragonruby-sprite-fs.el
│   ├── dragonruby-sprite-overlay.el
│   ├── dragonruby-sprite-completion.el
│   └── dragonruby-sprite-actions.el
├── dragonruby-mode.el       ;; ENTRY POINT
├── dragonruby-core.el       ;; CORE FACADE
├── dragonruby-colors.el     ;; COLOR FACADE
├── dragonruby-sprites.el    ;; SPRITE FACADE
├── dragonruby-paths.el      ;; PATH FACADE
├── dragonruby-image-tools.el;; IMAGE FACADE
├── dragonruby-concepts.el   ;; CONCEPT FACADE
└── dragonruby-docs.el       ;; DOCS SYSTEM (In Development)
```

## Module Breakdown

### 1. Core (`src/core/`)
**Responsibility**: Foundation of the universe.
- **Projects**: Knowing where `app/main.rb` is.
- **Assets**: Knowing that `.png` implies a `.aseprite` source exists nearby.
- **Utils**: Debounce function for efficient rescanning.

### 2. Colors (`src/colors/`)
**Responsibility**: Paint and Edit.
- **Scanner**: Detects `[255, 0, 0, 128]`, `0xFF...`, `{r:...}`.
- **Visuals**: Draws the background overlay AND the interactive `■` box.
- **Picker**: Handles user input to modify the code in-place, respecting the original format.

### 3. Paths (`src/paths/`)
**Responsibility**: Navigate code structure and data files.
- **Model**: Extension lists, snippet definitions.
- **FS**: Resolve paths, collect project files.
- **Snippets**: Expand `req` → `require ""`.
- **Overlay**: Underline clickable paths.
- **Actions**: Smart complete command, open project file.
- **NOTE**: Uses minibuffer (NOT CAPF) to avoid LSP conflicts.

### 4. Sprites (`src/sprites/`)
**Responsibility**: Asset visualization.
- **Model**: Supported image extensions.
- **FS**: Find sprites in project.
- **Overlay**: Inline thumbnails, clickable paths.
- **Completion**: CAPF for `sprites/` paths (depth 100, exclusive no).
- **Actions**: Jump to source file.

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
