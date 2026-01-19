# DragonRuby Mode Architecture (Lego System)

## 🏗️ Core Philosophy: Lego Architecture

The system (v0.7.2+) is built on a **Kernel-Driven Modular Architecture**.
It is not a monolithic plugin; it is a collection of independent modules ("Legos") orchestrated by a central Kernel.

## 🧠 The Kernel (`modules/core/dragonruby-kernel.el`)

The Kernel is the absolute authority. It manages:
1.  **Registry**: Which modules exist (`dragonruby-register-module`).
2.  **Lifecycle**: Enabling/Disabling modules (`dragonruby-enable`).
3.  **Dependency Resolution**: Ensuring `sprite-tools` logic waits for `sprites` logic.
4.  **Lazy Loading**: The Kernel manages the loading of implementation files (`--impl.el`) only when needed.

### The Three Laws
1.  **Namespace Law**: Every module MUST own its namespace (e.g., `dragonruby-sprite-`).
2.  **Capability Law**: Modules provide/require capabilities (e.g., `:rendering`, `:audio`).
3.  **Cold Boot Law**: The system starts with ZERO active modules.

## 📄 Manifest vs. Implementation Split

To ensure MELPA compliance and deterministic loading:
- **Manifests** (`dragonruby-<feature>.el`): Contain only registration and `autoloads`. They are always loaded at startup to register capabilities.
- **Implementations** (`dragonruby-<feature>--impl.el`): Contain all functions, variables, and modes. They are only loaded from disk when the module is activated.

## 🧱 Module Structure

All logic resides in `modules/`. The `src/` directory is dead.

```text
dragonruby-mode/
├── dragonruby-mode.el       ;; Entry Point (Bootloader)
├── modules/
│   ├── core/                ;; Kernel, Utils, Scheduler, Knowledge
│   ├── sprites/             ;; Sprite Rendering Engine
│   │   └── tools/           ;; Image Editor (Sub-module)
│   ├── fonts/               ;; Font Engine
│   │   └── tools/           ;; Font Tools (Sub-module)
│   ├── audio/               ;; Audio detection
│   ├── colors/              ;; Color palette system
│   ├── concepts/            ;; Educational overlays (Retina)
│   ├── paths/               ;; Navigation system
│   ├── completion/          ;; API Contract (YAML)
│   ├── guide/               ;; Knowledge Guidance (Memory)
│   └── stargate/           ;; Time Travel & Simulation control
│
├── knowledge/               ;; The Unified Brain (Content)
│   ├── definitions/         ;; Org files for concepts
│   └── feedback/            ;; User feedback storage
└── ...
```

## 🔄 Execution Flow (The Boot Sequence)

1.  **Load**: Emacs loads `dragonruby-mode.el`.
    *   It adds `modules/*` to `load-path`.
    *   It requires the **Kernel**.
    *   It requires all module **Manifests** (Lightweight registration).
2.  **Activation**: User runs `M-x dragonruby-mode` or auto-activation triggers.
    *   `dragonruby-mode` calls `(dragonruby-scheduler-enable)`.
    *   `dragonruby-mode` checks `defcustom` flags (e.g. `dragonruby-enable-sprites`).
    *   It calls `(dragonruby-enable 'sprites)` -> Kernel requires the **Implementation** (`--impl.el`) -> Module goes ONLINE.

## 🧘 Visual Policy (The "Good Citizen" Protocol)

DragonRuby Mode follows a strict "Observe and Paint" philosophy.
-   **Overlays**: We paint on top of text. We never modify buffer content.
-   **Debounce**: Scanning waits for user to pause typing.
-   **Zero Blocking**: Heavy assets (images) are loaded lazily on hover.

## 🛡️ CI Guardrail (The MELPA-Grade Guarantee)

To protect this modular architecture, the project adheres to a **Strict CI Standard**:
-   **Batch Verification**: Every push is verified with `emacs -Q --batch -L . -l dragonruby-mode.el`.
-   **Zero Intervention**: The plugin must handle its own `load-path` setup. If it doesn't work with only `-L .`, it is considered a bug.
-   **Layout-Agnostic Tests**: ERT tests must never assume a local path. They `require` the package and verify behavior in a clean environment.

---

*DragonRuby Emacs Mode — v0.7.4*
