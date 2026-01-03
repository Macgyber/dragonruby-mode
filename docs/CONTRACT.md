# DragonRuby Mode Contract

This document formalizes the responsibilities and interaction patterns for each module in `dragonruby-mode`. It serves as the single source of truth for the architecture, ensuring modularity and stability.

## Core Philosophy
1.  **Guiding Principle**: A module must fit entirely in your head. If you can't explain it in one sentence, it's too big.
2.  **Semantic First**: We provide semantic meaning (color, sprites, paths), not just syntax highlighting.
3.  **Modular Responsibility**: One module, one job. Modules should not leak responsibilities.
4.  **Keyboard First**: All features must be accessible via keyboard. Mouse is optional.
5.  **LSP-Safe**: Never interfere with LSP, Corfu, Company, or other completion frameworks.
6.  **Multi-Channel Async Strategy**: Periodic scans (Paths, Colors, Sprites) must use isolated debounce timers (`task-id`) to prevent cross-module collisions. Every asynchronous task must protect Emacs state using `save-match-data` and `save-restriction`.
7.  **Reactive Invalidation**: Any text modification in the buffer MUST immediately invalidate the visual overlays in the affected range to prevent "visual ghosts" or stale information. Reconstruction happens asynchronously during idle time.

---

## Module Contracts

### 0. Core Infrastructure (`src/core/`)
**Responsibility**: The immutable foundation.
*   **Projects (`dragonruby-project.el`)**: Reliable root detection (`app/main.rb`).
*   **Assets (`dragonruby-assets.el`)**: Knowledge of file types and source relations (`.png` -> `.aseprite`).
*   **Utils (`dragonruby-utils.el`)**: Debounce function for efficient rescanning.
*   **Events (`dragonruby-events.el`)**: Lightweight event bus.

### 1. Sprite System (`src/sprites/`)
**Responsibility**: Visualize and manage image assets.
*   **Structure**:
    *   `dragonruby-sprite-model.el`: Pure domain truth (validation, rules).
    *   `dragonruby-sprite-fs.el`: Filesystem logic (finding, resolving).
    *   `dragonruby-sprite-overlay.el`: Visuals + keyboard/mouse interaction.
    *   `dragonruby-sprite-completion.el`: CAPF logic (depth 100, exclusive no).
    *   `dragonruby-sprite-actions.el`: Jump to source file.
*   **Contract**:
    *   **Detection**: Parse strings starting with `sprites/`.
    *   **Visualization**: Inline thumbnails, color-coded underlines.
    *   **Interaction**: `RET`, `C-c RET`, `mouse-1` to open.
    *   **Preview**: `C-c p` for popup preview.
    *   **NON-INTERFERENCE**: CAPF uses `:exclusive 'no`, depth 100.

### 2. Path & Require System (`src/paths/`)
**Responsibility**: Navigate code structure and data files.
*   **Structure**:
    *   `dragonruby-path-model.el`: Extensions, snippet definitions.
    *   `dragonruby-path-fs.el`: Resolve paths, collect files.
    *   `dragonruby-path-snippets.el`: Expand `req` → `require ""`.
    *   `dragonruby-path-overlay.el`: Clickable underlines.
    *   `dragonruby-path-actions.el`: Smart complete, open file.
*   **Contract**:
    *   **Snippets**: `req`, `reqr`, `load`, `read`, `json` + `C-M-i`.
    *   **Completion**: Minibuffer-based (NOT CAPF) to avoid LSP conflicts.
    *   **Navigation**: `RET`, `C-c RET`, `mouse-1` to follow.
    *   **Open**: `C-c o` to open any project file.

### 3. Color System (`src/colors/`)
**Responsibility**: Visualize and Edit color usage.
*   **Structure**: `dragonruby-color-scanner`, `dragonruby-color-visuals`, `dragonruby-color-picker`.
*   **Contract**:
    *   **Detection**: RGB arrays, RGBA, hashes, hex.
    *   **Visualization**: Background overlay + interactive `■` box.
    *   **Interaction**: `RET`, `C-c RET`, `mouse-1` to edit.
    *   **Format Preservation**: Edits maintain original code format.

### 4. Image Tools (`src/image-tools/`)
**Responsibility**: Manipulate specific image assets.
*   **Structure**: `dragonruby-image-modify`, `dragonruby-image-view`, `dragonruby-image-ui`.
*   **Contract**:
    *   **Scope**: Active only when viewing an image file.
    *   **Tools**: Header-line buttons (Zoom, Rotate, Trim, etc.).
    *   **Safety**: Automatic backup with `[Undo]` button.

### 5. Concept System (`src/concepts/`) - In Development
**Responsibility**: Semantic connections.
*   **Contract**:
    *   **Detection**: Keywords (`args`, `state`, `tick`).
    *   **Visualization**: Subtle interactive underline.
    *   **Interaction**: `RET`, `mouse-1` opens documentation.

---

## Keyboard Navigation (Contract)

ALL interactive overlays MUST support:

| Key | Action |
|-----|--------|
| `RET` | Activate overlay (primary) |
| `C-c RET` | Activate overlay (alternative) |
| `mouse-1` | Click activation (optional) |

---

## System Architecture

```
src/
├── core/                <- Facade: dragonruby-core.el
├── sprites/             <- Facade: dragonruby-sprites.el
├── paths/               <- Facade: dragonruby-paths.el
├── image-tools/         <- Facade: dragonruby-image-tools.el
├── colors/              <- Facade: dragonruby-colors.el
├── concepts/            <- Facade: dragonruby-concepts.el
├── dragonruby-mode.el   <- Entry Point
└── dragonruby-docs.el   <- Knowledge System
```

## Compliance
- [x] **Architecture Verified**: All modules follow Facade/Submodule pattern.
- [x] **Keyboard Navigation**: All overlays support RET.
- [x] **LSP-Safe**: Paths uses minibuffer, Sprites CAPF at depth 100.
- [x] **Contracts Updated**: 2026-01-02.
