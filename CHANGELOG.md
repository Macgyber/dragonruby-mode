# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.5.5] - 2026-01-03

### 🧱 Final Modular Brick-Layer
- **Core Refactor Completeness**: Moved Events and Registry into `src/core/`. The project root is now the "Brick-Layer" for all others.
- **Dependency Elimination**: Absolute isolation between functional modules. Each part can be removed without affecting the mode's core stability.

### 🤝 Transparency & Community
- **Developer Mode Disclosure**: Added a "Micro-switches" section to the documentation for early-access testing.
- **GitHub Issue Templates**: Standardized forms for Feature Requests and Experimental Feedback.
- **Developer Documentation**: Revamped `CONTRIBUTING.md` to invite collaboration on "In Development" features.

### 🧹 Clean Binaries Policy
- **Byte-Compile Cleanup**: All `.elc` files are now deleted after audits/compilation tests to ensure a clean slate for users.

## [0.5.4] - 2026-01-03

### 🚧 User Safety & Clarity
- **"In Development" Guard**: Implemented a standardized interactive warning system. Attempting to use experimental or disabled features now triggers a professional notice in Spanish with a "Close" button.
- **Improved Fault Tolerance**: Activation errors in `dragonruby-mode` are now caught and reported via the interactive warning system instead of crashing silently.
- **Interactive Color Guard**: Clicking on color swatches when the picker is disabled now informs the user about the feature's development status.

## [0.5.3] - 2026-01-03

### 🏗️ Architecture (Strict Isolation)
- **Infrastructure Centralization**: Moved file extension definitions and project-wide asset knowledge to `src/core/dragonruby-assets.el`.
- **Zero-Dependency Modules**: Refactored `paths` and `sprites` modules to remove cross-dependencies. They now communicate exclusively through the Core infrastructure.
- **Backward Compatibility**: Maintained legacy aliases to ensure internal stability while moving to the new centralized model.

## [0.5.2] - 2026-01-03

### 🔍 Context-Aware Completion
- **Intelligent Filtering**: The Path completion system now recognizes code context (`.sprites`, `.labels`, `.require`, etc.).
- **Sprite-Only Mode**: Automatically filters out Ruby and Data files when a sprite-like path or context is detected, drastically reducing autocomplete noise.
- **Simplified UI**: Autocomplete lists are now shorter and 100% relevant to the current typing context.

## [0.5.1] - 2026-01-03

### 🖼️ Image Tools (Aesthetic & Logic Overhaul)
- **Fluid & Adaptive UI**: Implementation of a "Liquid" header-line. Buttons and labels now dynamically shrink (`VIEW` -> `V` -> 👁️) and spacings collapse based on window width to prevent UI overflow.
- **Accordion Logic**: Opening one tool group now automatically collapses others, ensuring a clean and focused workspace regardless of window size.
- **Timeline Navigation System**: Replaced the single "Undo" button with versioned `Back (<)` and `Forward (>)` buttons.
- **Non-Destructive History**: Every edit now saves a snapshot to a hidden `.dr_history` directory, allowing deep navigation of the edit history.
- **"Debug Stage" (Visual Ray-X)**: The `Info` button now toggles a high-contrast dark-gray background (`#333333`) to reveal transparent margins, facilitating precision trimming.
- **Active State Highlighting**: Group headers now "light up" (theme-colored background) when expanded, providing immediate visual feedback on the active context.
- **Workflow Reorganization**: Reordered buttons by priority. Navigation (`<`, `>`) moved to the `VIEW` group; `Info` and system tools unified under `SYSTEM`.

### ⚡ Performance & Reliability
- **Buffer-Revert Stability**: Fixed bug where groups would "explode" (auto-expand) after every image modification. Groups now default to collapsed state.
- **macOS Refresh Fix**: Forced instant buffer reload for Timeline navigation to ensure real-time visual updates.
- **Pixel-Art Preservation**: Updated resizing commands to use `-filter point` for razor-sharp scaling.

## [0.4.0] - 2026-01-02

### 🏗️ Architecture (Real-Time & Performance)
- **Multi-channel Debounce**: Independent timers for Paths, Colors, and Sprites. Prevents module collisions and ensures instant reactivity while typing.
- **Buffer-Local Project Cache**: Optimized project root detection to eliminate redundant Disk I/O during scans.
- **Atomic Scanning**: Implemented `save-match-data` and `save-restriction` in all periodic scans to prevent interference with user operations.
- **Micro-Module Logic**: Finalized the isolation of core modules for better error recovery.

### ✨ Features

#### 🗺️ Paths (Stabilized)
- **Hypertext Links**: Paths are now blue bold links. Validated instantly (50ms - 100ms) after typing.
- **Universal CAPF**: Completions list all project files without restrictive filtering.
- **Snippet Overhaul**: Added `spr` (sprite) and `script` (load_script) snippets. Corrected cursor positioning inside quotes.

#### 🎨 Colors
- **Visual Swatches**: Swatches scale with font size and provide feedback on transparency.
- **Picker Feature Flag**: Added `dragonruby-enable-picker` (default `nil`). Disabling the interactive picker while it is being perfected to avoid confusion.

#### 🖼️ Image Tools
- **ImageMagick Unified Check**: Commands now automatically detect if ImageMagick is missing and show an interactive menu with a direct download link.
- **Improved UI**: Applied premium button styling to the image header-line.
- **Undo System**: Integrated automatic backups before any destructive ImageMagick operation.

### 🐛 Fixed
- **Timer Collisions**: Fixed bug where color scanning would cancel path detection.
- **Buffer Affinity**: Debounced functions now strictly execute in the correct buffer.
- **Stale Code**: Purged all `.elc` files to ensure only the latest source is active.
- **Syntax Integrity**: Resolved `end-of-file` errors in `dragonruby-utils.el`.

### 📚 Documentation
- **README Refactor**: Updated with the new Hypertext and Architecture details.
- **Contract Update**: Formalized the "Fault-Tolerant Multi-channel" rule.

---

## [0.3.0] - 2025-12-29

### 🏗️ Architecture (Major Refactor)
- **Modular Core**: Split monolithic files into focused sub-modules (`src/core/`, `src/paths/`, `src/colors/`, `src/image-tools/`).
- **Facade Pattern**: Main modules now act as facades orchestrating specialized logic.
- **Enterprise-Grade Structure**: Cleaned up dependencies and enforced "One Module, One Responsibility".

### ✨ New Features
- **Advanced Color System**:
    - **Alpha Support**: Detects and visualizes transparency in arrays (`[r,g,b,a]`) and hashes (`{r:_, a:_}`).
    - **Color Picker**: Added interactive "Edit Color" button (`■`) next to color values.
    - **Accessibility**: Color swatches scale with font size and support local editing.
- **Concept Visuals**:
    - New module `src/concepts` scanning for DragonRuby keywords (`args`, `state`, `tick`).
    - Subtle, non-invasive interactive underlines connecting to documentation.
- **Image Tools**:
    - Full modularization of image modification and viewing tools.

### Fixed
- **Byte-Compile**: Validated entire codebase to be clean of byte-compile errors.
- **Docstrings**: Standardized format across all modules.

## [0.2.1] - 2025-12-28

### Added

- **Autocomplete**: Enhanced `require` autocomplete (`req + C-M-i`) to intelligently suggest `.rb` files. Added support for data file completion (json, txt, csv) in string contexts.
- **Improved Sprites**: Sprite autocomplete (`spr + C-M-i`) now inserts proper string paths and shows `🖼️` icons. 
- **Robustness**: Implemented a "Feature Flag" system to enable/disable specific modules individually.
- **Interactivity**: Fixed sprite overlays to be fully clickable (supports Mouse-1, Mouse-2, Enter) using `follow-link` property.
- **Configuration**: Exposed `dragonruby-unsupported-sprites` and `dragonruby-data-extensions` as customizable user options (`defcustom`).

### Experimental (Disabled by Default)
- **Living Documentation**: System to link code symbols to local `.org` concepts.
- **Smart Source Jumping**: Ability to open `.aseprite` files from the image viewer.

## [0.1.0] - 2025-12-25

### Added
- Initial release of **dragonruby-mode**.
- Semantic highlighting for Colors (RGB arrays, hashes, hex).
- Sprite previews (inline thumbnails and hover tooltips).
- Basic path navigation (clickable file paths).
- Automatic project detection (`app/main.rb`).
