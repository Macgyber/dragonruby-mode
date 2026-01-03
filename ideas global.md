# ANTIGRAVITY INTERNAL STATE: learnDR-mode Master Plan
> **NOTE:** This is the internal representation of the implementation plan designated as `.learndr-plan.md` within the agent's context. It contains the logic, status, and architectural intent for the DragonRuby Learning Mode.

## 📡 CURRENT STATUS SUMMARY
| Phase | Description | Status | Agent Action |
|-------|-------------|--------|--------------|
| **1** | Paths & Colors Stability | 🟢 **DONE** | Multi-channel debounce + Hypertext links functional. |
| **2** | Sprite Previews Refinement | 🟢 **DONE** | Inline thumbnails + scale with font-size. |
| **3** | learnDR-mode (Docs System) | 🟡 **IN PROGRESS** | Integrating Org-mode concepts as hyperlinks. |
| **4** | Image Tools UI | 🟢 **DONE** | Fluid UI + Timeline Navigation + Debug Stage. |
| **5** | Semantic Concepts | 🟡 **IN PROGRESS** | Highlighting `args`, `state`, etc. |
| **6** | Advanced Color Picker | 🔴 **PENDING** | Disabled (nil) via flag until refined. |

---

## 🧠 PHASE 1: CANONICAL CONCEPTS (Immutable Truth)
*Logic: The system must strictly distinguish between DragonRuby API (the "Engine") and standard Ruby or User code.*

### Validated Symbols Table
| ID | Symbol | Document Target | Category | Type |
|----|--------|-----------------|----------|------|
| 1 | `tick` | `docs/engine/tick.org` | Engine | **DR** |
| 2 | `args` | `docs/engine/args.org` | Engine | **DR** |
| 3 | `args.inputs` | `docs/context/inputs.org` | Context | **DR** |
| 4 | `args.outputs` | `docs/context/outputs.org` | Context | **DR** |
| 5 | `args.state` | `docs/context/state.org` | Context | **DR** |
| 6 | `args.grid` | `docs/context/grid.org` | Context | **DR** |
| 7 | `args.audio` | `docs/context/audio.org` | Context | **DR** |
| 8 | `Sprite` | `docs/render/sprite.org` | Render | **DR** |
| 9 | `Label` | `docs/render/label.org` | Render | **DR** |
| 10 | `Solid` | `docs/render/solid.org` | Render | **DR** |
| 11 | `Border` | `docs/render/border.org` | Render | **DR** |
| 12 | `attach` | `docs/dsl/attach.org` | DSL | **DR** |
| 13 | `scene` | `docs/dsl/scene.org` | DSL | **DR** |
| 14 | `entity` | `docs/dsl/entity.org` | DSL | **DR** |
| 15 | `enemy` | `docs/dsl/enemy.org` | DSL | **DR** |

> **AGENT CONSTRAINT:** `player`, `hero`, `calc` are strictly USER variables/functions and must NEVER be highlighted as concepts.

---

## 📂 PHASE 2: DOCUMENTATION TOPOLOGY
*Logic: File structure mirrors the mental model of the engine.*

```text
docs/
├── engine/      # The Runtime Loop
│   ├── tick.org
│   └── args.org
├── context/     # The World Data
│   ├── inputs.org
│   ├── outputs.org
│   ├── state.org
│   ├── grid.org
│   └── audio.org
├── render/      # The Visuals (Primitives)
│   ├── sprite.org
│   ├── label.org
│   ├── solid.org
│   └── border.org
└── dsl/         # The Patterns (Higher Order)
    ├── attach.org
    ├── scene.org
    ├── entity.org
    └── enemy.org
```

---

## 🛠️ PHASE 3: CONTENT GENERATION (Next Steps)
*Goal: Populate the empty `.org` shells with specific DragonRuby pedagogical content.*

**Execution Order:**
1.  **Core Engine**: `tick` (The Heart) -> `args` (The Blood).
2.  **Rendering**: `Sprite` (Most common) -> `Label`.
3.  **Input/State**: `args.inputs` -> `args.state`.

**Content Template for `.org` files:**
-   **Definition**: One sentence summary.
-   **Signature**: Ruby method signature.
-   **Visual Metaphor**: How to think about it (e.g., "The Tick is a Frame").
-   **Code Example**: Minimal, copy-pasteable snippet.
-   **Common Pitfalls**: "Don't do X".

---

## 🔮 PHASE 4: XREF NAVIGATION ARCHITECTURE
*Goal: `M-.` should work on `args.outputs` even if no LSP is running.*

**Implementation Plan:**
1.  **Backend**: create `dragonruby-xref-backend` in `src/dragonruby-docs.el`.
2.  **Detection**: Regex scan for the 15 canonical symbols.
3.  **Resolution**: Map symbol -> Filepath (`docs/...org`) -> Line 1.
4.  **Fallback**: If not a DR symbol, yield to `etags` or `eglot` (LSP).

---

## 🔮 PHASE 5: SEMANTIC OVERLAYS (The "Pedagogical Layer")
*Goal: Visual distinction of the Framework vs the Language.*

**The "Overlay Two" System:**
1.  **Concept Overlay (Cyan/Hyperlink)**:
    -   Targets: The 15 canonical symbols.
    -   Action: Click/Enter opens local `.org` docs.
    -   Meaning: "This is DragonRuby."
2.  **Ruby Overlay (Pink/Subtle)**:
    -   Targets: `def`, `class`, `module`, `do...end`.
    -   Meaning: "This is pure Ruby."

**Technical Implementation:**
-   Use `make-overlay` with a priority higher than syntax highlighting.
-   Properties: `face`, `help-echo`, `keymap`, `evaporate: t`.
-   Trigger: `jit-lock` or `post-command-hook` (debounced).

---

## 🧠 INTERNAL AGENT DIRECTIVES
1.  **Text is Truth**: Never modify the user's code for "features". Overlays are visual only.
2.  **No Magic**: Every jump must be explicable (e.g., "I jumped to `sprite.org` because you clicked `Sprite`").
3.  **Modularity**: Keep the `docs` module decoupled from the `sprites` module where possible, though they share the "DragonRuby" context.
