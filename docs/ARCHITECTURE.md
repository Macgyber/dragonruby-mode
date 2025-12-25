# Architecture & Design

This document describes the internal structure of **DragonRuby Mode** (`v0.2.0`).

## Core Philosophy: Semantic Overlays

The mode operates strictly on **overlays**. It never modifies buffer text unless the user explicitly commits an edit (e.g., changing a color value). It follows the **Zero-UI** principle: no popups, no sidebars, no modal dialogs. Use the buffer itself as the UI.

## Module Breakdown

### 1. `dragonruby-mode.el` (Orchestrator)
- Defines the minor mode.
- Manages hooks (`after-change-functions`, `completion-at-point-functions`).
- **Optimization**: Sets a local `tooltip-delay` of `0.1s` for instant feedback without affecting global Emacs settings.
- Runs the scanning loop: `Scan Colors` -> `Scan Sprites` -> `Scan Paths`.

### 2. `features/dragonruby-colors.el`
- **Regex Parsing**:
  - Arrays: `\[\s*(0x[0-9a-f]+|\d+), ... \]`
  - Hashes: Finds blocks like `r: 10, g: 20...` allowing arbitrary key order.
- **Overlay**: Uses `(:background HEX :foreground CONTRAST)` face.
- **Interaction**: Uses `read-number` in the minibuffer for non-intrusive editing.

### 3. `features/dragonruby-sprites.el`
- **Regex**: Matches `"path/to/image.ext"`.
- **Validation**:
  - Checks if file exists on disk relative to Project Root.
  - Checks extension against allow-list (`png`, `jpg`, `bmp`, `gif`).
- **Tooltip**: Generates a rich string with:
  - Header: `📏 WxH px (EXT) SIZEkb` (using `identify` or `sips`).
  - Body: Image thumbnail (`create-image`).

### 4. `features/dragonruby-paths.el`
- **Require Linking**:
  - Matches `require "..."`.
  - Resolves path relative to `app/` (standard DragonRuby behavior).
- **Auto-completion**:
  - Hooks into `completion-at-point`.
  - Scans `app/`, `sprites/`, `audio/` recursively.
  - Triggered automatically on `/` inside strings.

## Project Resolution (`core/dragonruby-project.el`)
- Finds the Project Root by looking for the `app/` folder upwards.
- All asset resolution is relative to this root.
