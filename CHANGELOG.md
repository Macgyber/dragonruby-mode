# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

---

<details open>
<summary><h2>[1.0.0] - 2026-01-19 (STARGATE v1.0 STABLE: SESSION INTEGRITY)</h2></summary>

### 🌌 Stargate v1.0 Stable (Hardened)
- **Universal Address Integrity**: Implemented `session_id@branch@tick` globally. History is now immutable and immune to cross-session contamination.
- **Deterministic Data**: Purged physical time (`observed_at`) from the core logic. The simulation now relies exclusively on deterministic hashes and seeds.
- **Failsafe FSM**: The Finite State Machine in `manager.el` now strictly prevents operational leaps (Record/Jump/Fork) unless the system is in a verified stable interposition.
- **Hierarchical Shadowing**: Finalized the inheritance engine. Branches correctly shadow parent moments while maintaining perfect isolation for divergent frames.
- **Meta-Programming Detection**: The Injector now detects risky Ruby patterns (`eval`, `send`, `define_method`) as **GAMMA** changes, requiring user confirmation for irreversible mutations.
- **Historical Audit**: External filesystem mutations are now recorded as physical events in the session index, making "time leakage" fully observable.
- **Automated Verification**: Established a dedicated `verify_stargate_inheritance.el` suite to guarantee the integrity of the temporal engine.

### 🛡️ Core & System
- **Hardened Git Privacy**: Specialized `.gitignore` logic to exclude private debug registries, test logs, and experimental artifacts while keeping the public codebase clean.
- **Global Sentinel**: Added defensive `boundp` and `fboundp` guards across the Stargate module to ensure stability even when bridge components are partially loaded or unavailable.

</details>

<details>
<summary><h2>[0.8.1] - 2026-01-18 (STARGATE STABILIZATION: THE INDUSTRIAL LEAP)</h2></summary>

### 🌌 Stargate Stabilization (Deployment Grade)
- **Portal Strategy**: Implemented a sandbox-compliant injection mechanism using a temporary `stargate_portal.rb` in `mygame/`. This bypasses DragonRuby's file restrictions and ensures 100% loading reliability.
- **Void Shield (Anti-Lag Storm)**: Developed a high-speed selective log processor that vaporizes massive bursts of engine noise (like Render logs during resize) before they touch the Emacs UI. Zero blocking during window operations.
- **Lifecycle-Aware Injection**: The Bridge now actively monitors for the `RNG seed` signal, ensuring the VM is stable and ready before attempting any injection.
- **Deterministic Single Injection**: Implemented a global "Infection Requested" lock in Emacs. Guarantees that the injection command is sent exactly once per stable session, eliminating re-entry bugs and minibuffer errors.
- **Industrial Runtime Refactor**:
  - **Standard Alias Protection**: Real idempotency in `bootstrap.rb` using explicit `method_defined?` checks to prevent infinite recursion on hot reloads.
  - **Fallback Strategy**: Added fallback support for projects without a pre-defined `tick` method.
  - **Segregated States**: Separate logic for technical installation (`$stargate_installed`) and operational readiness (`$stargate_operational`).

### 🧠 Project Context & Utility
- **Robust Root Anchoring**: Refined `dragonruby--anchor-project-root` to reliably identify the parent of `mygame/`, ensuring all session assets are stored in the correct project context.

</details>

<details>
<summary><h2>[0.8.0] - 2026-01-17 (STARGATE PHASE 5: FORKS & REPLAY)</h2></summary>

### 🌌 Stargate Phase 5: Forks & Replay
- **O(1) Branching**: Implemented `stargate-fork` for metadata-only branch creation without data duplication.
- **Hierarchical Inheritance**: Developed recursive moment retrieval with shadowing support (local branch overrides parent).
- **Deterministic Replay**: Enhanced Timeline Jump to resolve inherited state correctly using the recursive getter.
- **Protocol v1.0 Standard**: Finalized and enforced internal string-key mappings across all session data structures.

### 🌌 Stargate Persistence (Layer 2)
- **Schema v1**: Formalized the `.dr-stargate` JSON index with explicit versioning and nested temporal schema (`tick`/`monotonic_ms`).
- **Session Lifecycle**: Implemented `on-idle` (30s deferred), `on-exit` (atomic flush), and `on-demand` persistence.
- **Session Recovery**: Built `dragonruby-stargate-session-load` for O(1) restoration of historical hash tables from disk.

### 🛡️ Guardian Module (Layer 3)
- **Robust Hashing**: Optimized SHA-256 verification in `tracker.el` to ignore non-semantic whitespace, eliminating false-positive stasis triggers.
- **Pause Reliability**: Verified immediate `Clock.pause!` invocation upon authoritative buffer mismatch.

### 🧠 Mind & Determinism (Layer 4)
- **Verification Compliance**: Enforced nested `observed_at` validation across the whole IPC bridge to ensure state reproducibility.
- **Timeline Consumer**: Decoupled rendering logic from capture state; the Timeline now operates as a passive observer of the session index.

### 🎨 UX & Polish
- **Contextual CAPF**: Refined path completion exit-function to correctly position the cursor post-insertion.
- **Lazy Visuals Toggle**: Introduced `dragonruby-sprites-lazy-visuals` to allow users to opt-out of automatic sprite thumbnailing.

</details>

<details>
<summary><h2>[0.7.4] - 2026-01-16 (CORE ARCHITECTURE & IMPROVED CI)</h2></summary>

### 🛡️ Artisan CI (MELPA-Grade Integrity)
- **CI Technical Standard**: Drafted and implemented a formal, isolated environment standard for all future contributions.
- **Strict Load-Path**: Enforced `-L .` constraint to ensure the package is architecturally self-sufficient.
- **Test Standardization**: Modernized 13+ test files to be layout-agnostic, relying exclusively on internal module resolution.
- **GitHub Actions**: Deployed the official CI workflow on Ubuntu-latest with Emacs 29.1.

### 🏛️ Core Architecture Refactor (Performance Fix)
- **Project Root Anchor**: Implemented one-time project root detection per buffer. Eliminated recursive upward disk scans in idle pulses.
- **Lazy Sprite Overlays**: Visual metadata and image creation are now deferred to hover time. 0% CPU impact during idle pulses.
- **Architectural Throttling**: Increased idle delay to 1.5s. Implemented global guards that abort background logic if no project root is anchored.
- **Memory Hygiene**: Sanitized the Kernel ledger to eliminate defunct timers and hooks.
- **Emergency Recovery**: Introduced `M-x dragonruby-kernel-system-halt` (alias `stargate-halt`) for immediate system recovery.

### 🌌 Stargate: Time-Travel & Determinism
- **Metadata IPC**: Reduced bridge traffic by 90% using telemetry sampling (record every 10 frames).
- **Native Disk Storage**: Game states are now saved directly by the engine. Emacs only tracks hashes and seeds.
- **Divergence Detection**: Automatic detection of non-deterministic states with rollback warnings.
- **Aliases**: New ultra-short aliases for all major commands: `stargate-timeline`, `stargate-enable`, etc.

</details>

<details>
<summary><h2>[0.7.3] - 2026-01-12 (CORE KERNEL & RELIABLE RELOAD)</h2></summary>

### 🧠 Unified Micro-Kernel (System Architecture)
- **Central Life Registry**: Implementation of a global "Registry Book" for Timers, Hooks, and Processes. No resource can be born without being registered by the Kernel.
- **Atomic Shutdown**: The shutdown system (`system-shutdown`) is now an atomic operation that guarantees the death of all old activity before any change.
- **Zombie Hunt**: Safety net that scans the global Emacs timer list to cancel orphan functions from the `dragonruby-` namespace.

### 🔄 Reliable Hot-Reload (F6)
- **OS Life Cycle**: Implementation of the `Shutdown -> Unload -> Load -> Reboot` flow.
- **Symbol Integrity**: Destructive `mapatoms` purging was removed. Now symbols remain but code is surgically refreshed without corrupting the global Emacs state.
- **Atomic Reload**: Guaranteed background reload of critical modules (Kernel, Scheduler, Audio) to apply logic changes instantly.

### 🔊 Audio & Diagnostics (Visual Upgrade)
- **Audio Metadata**: Real duration extraction (via `afinfo/mdls`) with high-speed caching system.
- **0-Byte Validation**: The system now detects and marks empty or corrupt audio files as invalid.
- **Dynamic Hover**: Redesigned audio tooltip with optimized classic format (duration, weight, type) and improved error feedback.

### 🖼️ Scan Optimization
- **Visible Retinal Vision**: Sprites, Audio, Fonts, and Colors modules now operate exclusively in the visible region (with 3000 chars padding), eliminating lag in files with thousands of lines.

### 🌌 Stargate Module (Time Travel) — EXPERIMENTAL
- **Kernel Organ**: First module designed as a "living organ" that beats synchronized with the Kernel Heartbeat (Scheduler).
- **Session Recorder (Bridge)**: JSON pulse reassembly via console for real-time simulation moment capture.
- **Code Injector**: Atomic injection system with risk classification (Alpha/Beta/Gamma) and automatic reversion.
- **Temporal Navigation**: Visual visualization of the "Forest of Branches" and capability of instant jump between historical states.
- **Forced Determinism**: RNG and state capture tools to guarantee total reproducibility of the simulation.

</details>

<details>
<summary><h2>[0.7.2] - 2026-01-12 (KNOWLEDGE BASE & STABILITY)</h2></summary>

### 📖 Guide Module (KNOWLEDGE BASE)
- **Guide Aesthetics**: Org-mode visual transformation with hierarchical titles (1.4x), intelligent margins, and visual ellipsis (`▾`).
- **RET Navigation**: Implemented direct jump to documentation by pressing `RET` over concepts in the code with optimized memory management.
- **Right Sidebar**: Persistent and anchored lateral panel (`no-delete-other-windows`) for knowledge consultation without interruptions.
- **Wisdom Lathes**: `args.org`, `state.org`, and `sprite.org` volumes structured under the new encyclopedic aesthetic.

### 🎨 Colors Module (TESTED & STABLE)
- **Solid Bar Strategy**: Compact rendering that merges visual components, eliminating noise in commas and spaces.
- **Object Isolation**: Proximity logic and restart by duplicity to avoid mixing colors between nearby variables.

### 💓 Stability and Engineering (Zero-Blocking)
- **Overlay Cleanup**: Total reengineering of the overlay life cycle. Elimination of memory leaks and dynamic closures that caused blockages.
- **Smart Scheduler**: Single-shot beat at 0.5s guaranteeing smooth scroll even in massive files.
- **Hot-Reload (F6)**: Purified global restart cycle, `load-path` reconstruction, and atomic buffer reload.

</details>

<details>
<summary><h2>[0.7.1] - 2026-01-10 (TOTAL SYNC & SMART DOT)</h2></summary>

### 📜 Unified Configuration Standard
- **Ultra-Minimalist Core**: New factory standard where **all** optional modules (`colors`, `sprites`, `fonts`, `audio`, etc.) are disabled by default, **except completion**. The plugin now starts as a purely productivity tool.
- **Code-Doc Parity**: Absolute synchronization between the code's `defcustom` and the "Lego Piece" tables in the README. No fine print.
- **Smart Dot**: Autocompletion triggers automatically when typing a dot, **only** if it follows a valid contract string (e.g., `args.`). Radical improvement of writing flow.

### 🧠 Native Intelligence
- **Fallback Detection**: The system now automatically detects the global `dragonruby_api.yml` in the plugin folder if no local one exists.
- **Shortcut Redundancy**: Explicit support added for `C-M-i` alongside the move friendly `C-.`.
- **Auto-Dot Insertion**: When completing a root (like `arg` -> `args`), the system automatically inserts the dot to continue the chain.

### 🐛 Bug Fixes
- **Obsolete Modes**: Fixed checks to minor modes that no longer exist in Lego architecture:
  - `dragonruby-font-overlay.el`: `dragonruby-font-mode` → `dragonruby-mode`
  - `dragonruby-audio-overlay.el`: `dragonruby-audio-mode` → `dragonruby-mode`
  - `dragonruby-sprite-overlay.el`: `dragonruby-sprite-mode` → `dragonruby-mode`
  - `dragonruby-concept-visuals.el`: `dragonruby-concepts-mode` → `dragonruby-mode`
- **Path Overlay**: Fixed typo `dragonruby-data-extensions` → `dragonruby-data-exts`.
- **dragonruby-utils.el**: Corrected `dragonruby--get-image-type` function with missing parenthesis.

</details>

<details>
<summary><h2>[0.7.0] - 2026-01-09 (PHASE 5: LEGO ARCHITECTURE & KERNEL)</h2></summary>

### 🏗️ Lego Architecture (The Kernel)
The system has been restructured from scratch. It is no longer a collection of scripts; it is a modular **Operating System**.
- **The Kernel**: A central orchestrator that manages the life and death of each functionality.
- **The Three Core Policies**:
  1. **Namespace Law**: Each module owns its exclusive space.
  2. **Capability Law**: Modules declare what they *need* (e.g., `:rendering`) and what they *provide*, not who they know.
  3. **Cold Boot Law**: Nothing runs by default. Zero zombies.

### 🛡️ Total Modularity
All systems have been encapsulated in `modules/` with strict contracts (`manifest`):
- `modules/core`: Kernel and base libraries.
- `modules/sprites`: Rendering engine.
- `modules/sprites/tools`: Image editor (depends on sprites).
- `modules/fonts`: Typography viewer.
- `modules/audio`, `modules/colors`, `modules/paths`, `modules/concepts`.

### 📦 MELPA Preparation
- Compatible structure with standard packaging.
- `dragonruby-pkg.el` added.
- Flexible configuration: The user can deactivate individual pieces (`legos`) in their `init.el`.

</details>

<details>
<summary><h2>[0.6.1] - 2026-01-08 (PHASE 4: CONTRACT COMPLETION & FLUIDITY)</h2></summary>

### 🧠 Native Intelligence (Autocomplete Engine)
The autocompletion system has been stabilized and verified.
- **Auto-Dot Flow**: When selecting a root like `args`, the system automatically inserts the dot (`args.`), allowing fluid writing (`args.` -> `state`).
- **Native Data**: Implemented as a standard CAPF backend. 100% compatible with `Minibuffer`, `Company-Mode`, and `Corfu`.
- **Zero-Ghost Policy**: Code audited and purged of references to obsolete modules. Debugging logs removed for maximum performance.
- **Namespace Safety**: Backend renamed to guarantee zero collisions with other utilities.
- **Contract Fallback**: Searches for `dragonruby_api.yml` in the project root; if it doesn't exist, it uses a global backup contract.

### 🏭 Technical Improvements
- **Windows Shortcut**: Implemented `C-.` as a native shortcut to trigger autocompletion in DragonRuby Mode.
- **Silent Core**: Visual label `[Contract]` removed for a cleaner and more native UI integration.

</details>

<details>
<summary><h2>[0.6.0] - 2026-01-06 (INDUSTRIAL PHASE: ZERO BLOCKING & RELIABILITY)</h2></summary>

### 🏭 Industrial Shielding ("Outside vs Inside")
This version represents a complete reengineering under the philosophy of **"Invisible Order"**.

### Added
- **Active metrics on load**:
  - The activation system (`dragonruby-mode.el`) now reports critical errors with surgical precision (`CRITICAL FALLBACK`), allowing immediate diagnostics.
  - Elimination of silent failures in module loading.

### Improved
- **Zero Blocking (Extreme Performance)**:
  - **Sprites Refactor**: Removed *all* image generation from the main scan thread.
  - **Result**: Scanning large files is now instant (~0ms blocking). Rich previews (full images) are loaded *lazy* only on hover (200ms), keeping the editor "light as silk."
- **Controlled Memory (SRE)**:
  - **Singleton Timer Pattern**: Implemented strict timer control in sprite popups.
  - **Leak Prevention**: It is guaranteed that only one timer exists at a time, eliminating the risk of "Timer Storms" when moving the mouse quickly.
  - **Clear Cycles**: Deactivating the mode now aggressively cleans up all visual resources and pending processes.
- **Silent Core**:
  - **Defense in Depth**: `dragonruby-project.el` now handles null contexts (buffers without a file) without throwing exceptions, guaranteeing total stability in scratchpads and terminals.

### Changed
- **Separation of Visual Responsibilities**:
- **Inline**: Only shows cached mini-thumbnails (Instant validation).
- **Popup**: Exclusively handles the loading of rich media (Detail on demand).
- This separation is the key to the new "Zero Blocking" architecture.

</details>

---

<details>
<summary><h2>[0.5.0] - 2026-01-05 (PHASE 3: AUDIO, FONTS & POLISH)</h2></summary>

### Added
- **DragonRuby Creative Hub (v1)**:
  - Full integration with external editors.
  - Web buttons for Graphite, Piskel, Lospec, Itch.io.
  - "Adaptive UI" system that changes based on window width.
  - Users can add their own "Creative Tools" (URLs or Exes).
- **Audio System (Experimental)**:
  - `args.audio` detection.
  - Basic `.wav`/`.ogg` playback from Emacs (depends on backend).
- **Fonts**:
  - `.ttf` preview on hover.
  - Font installation (placeholder).

### Improved
- **Image Editor**:
  - Added "Rotate 90°" button.
  - Added "Flip H/V" button.
  - Improved responsive layout (VIEW, TRANSFORM, COLOR, SYSTEM, CREATIVE).

</details>

<details>
<summary><h2>[0.4.0] - 2026-01-04 (PHASE 2: PATHS & REFACTOR)</h2></summary>

### Added
- **Path System (Navigation)**:
  - Intelligent detection of `require`, `read_file`, `write_file`.
  - JSON and CSV string detection.
  - Clickable links (Open file).
- **Modular Refactor**:
  - Strict separation: `core`, `sprites`, `paths`, `colors`.
  - Elimination of circular dependencies.
  - Deferred loading (autoloads).

</details>

<details>
<summary><h2>[0.3.0] - 2026-01-03 (PHASE 1: SPRITES & COLORS)</h2></summary>

### Added
- **Sprite System**:
  - Inline thumbnails (font size).
  - Basic tooltip.
- **Color System**:
  - RGB Array detection `[255, 0, 0]`.
  - Hash detection `{r: 255, ...}`.
  - Real color overlay.

</details>

<details>
<summary><h2>[0.1.0] - 2026-01-01 (START)</h2></summary>

### Added
- Project base structure.
- `dragonruby-mode.el` (skeleton).
- Basic detection of `.rb` files.

</details>
