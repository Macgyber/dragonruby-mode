# DragonRuby Mode - Architecture Documentation

## Core Philosophy
This minor-mode follows a **Semantic First** approach. It strictly avoids UI clutter (popups, sidebars) in favor of inline overlays that enhance the text without hiding it.

## Module Structure

### `src/dragonruby.el`
The entry point. Responsibilities:
- Sets up the `load-path`.
- Requires the core modules in order (`project`, `features`, `mode`).
- Provides the `dragonruby` feature.

### `src/dragonruby-mode.el`
The Minor Mode definition (`define-minor-mode`). Responsibilities:
- Activates/Deactivates the mode (`dragonruby-mode`).
- Manages global hooks:
    - `after-change-functions`: Triggers rescans when text is edited.
    - `completion-at-point-functions`: Registers semantic autocompletion.
- Orchestrates the scanning loops (Scan Colors → Scan Sprites → Scan Paths).

### `src/features/`
Each feature is an independent "contract" that implements a specific semantic behavior.

#### 1. Color Semantics (`dragonruby-colors.el`)
- **Detection**: Regex matching `[r, g, b, a?]` arrays (decimal and hex support).
- **Visualization**: Overlays the text with the actual color background.
- **Interaction**: Click/Command to edit values dialog-free (minibuffer interaction).

#### 2. Sprite Semantics (`dragonruby-sprites.el`)
- **Detection**: Regex matching strings ending in `.png`, `.jpg`, etc.
- **Visualization**: Interactive overlay (clickable).
- **Feedback**:
    - Underline Cyan: File exists.
    - Wavy Red: File missing.
    - Hover: Shows image thumbnail and metadata (dimensions, size).

#### 3. Path Navigation (`dragonruby-paths.el`)
- **Detection**: `require` statements.
- **Interaction**: Click to jump to file.
- **Autocompletion**: Context-aware `completion-at-point` for local project files.

## Project Resolution
The system relies on finding the project root (usually denoted by `app/main.rb` or `.git`). All paths are resolved relative to this root.
