# DragonRuby Mode Architecture (Lego System)

## 🏗️ Core Philosophy: Lego Architecture

The system (v0.7.1+) is built on a **Kernel-Driven Modular Architecture**.
It is not a monolithic plugin; it is a collection of independent modules ("Legos") orchestrated by a central Kernel.

## 🧠 The Kernel (`modules/core/dragonruby-kernel.el`)

The Kernel is the absolute authority. It manages:
1.  **Registry**: Which modules exist (`dragonruby-register-module`).
2.  **Lifecycle**: Enabling/Disabling modules (`dragonruby-enable`).
3.  **Dependency Resolution**: Ensuring `sprite-tools` logic waits for `sprites` logic.

### The Three Laws
1.  **Namespace Law**: Every module MUST own its namespace (e.g., `dragonruby-sprite-`).
2.  **Capability Law**: Modules provide/require capabilities (e.g., `:rendering`, `:audio`).
3.  **Cold Boot Law**: The system starts with ZERO active modules.

## 🧱 Module Structure

All logic resides in `modules/`. The `src/` directory is dead.

```text
dragonruby-mode/
├── dragonruby-mode.el       ;; Entry Point (Bootloader)
├── modules/
│   ├── core/                ;; Kernel, Utils, Scheduler
│   ├── sprites/             ;; Sprite Rendering Engine
│   │   └── tools/           ;; Image Editor (Sub-module)
│   ├── fonts/               ;; Font Engine
│   │   └── tools/           ;; Font Tools (Sub-module)
│   ├── audio/               ;; Audio detection
│   ├── colors/              ;; Color palette system
│   ├── concepts/            ;; Educational overlays
│   ├── paths/               ;; Navigation system
│   ├── completion/          ;; API Contract (YAML)
│   └── docs/                ;; Documentation linkage
└── ...
```

## 🔄 Execution Flow (The Boot Sequence)

1.  **Load**: Emacs loads `dragonruby-mode.el`.
    *   It adds `modules/*` to `load-path`.
    *   It requires the **Kernel**.
    *   It requires all module entry points (Manifest Registration).
2.  **Activation**: User runs `M-x dragonruby-mode`.
    *   `dragonruby-mode` calls `(dragonruby-scheduler-enable)`.
    *   `dragonruby-mode` checks `defcustom` flags (e.g. `dragonruby-enable-sprites`).
    *   It calls `(dragonruby-enable 'sprites)` -> Kernel resolves dependencies -> Module goes ONLINE.

## 🧘 Visual Policy (The "Good Citizen" Protocol)

DragonRuby Mode follows a strict "Observe and Paint" philosophy.
-   **Overlays**: We paint on top of text. We never modify buffer content.
-   **Debounce**: Scanning waits for user to pause typing.
-   **Zero Blocking**: Heavy assets (images) are loaded lazily on hover.

---

*DragonRuby Emacs Mode — v0.7.1*
