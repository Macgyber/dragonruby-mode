# The DragonRuby Mode Contract

**Status:** v1.0 — The Invisible Order.

This document defines the underlying philosophy that governs the entire `dragonruby-mode` project. From the first color highlight to the final autocomplete suggestion, every feature obeys this single Contract.

## 🏛️ The Core Philosophy

**"Clarity over Cleverness. Truth over Magic."**

We believe an editor should be a lens, not a filter. It should reveal the reality of your code with absolute precision, without trying to "guess" your intent or obscure the details.

### The Pillars
1.  **Determinism**: If X happens, it is because Y is strictly met. No random behaviors.
2.  **No Hallucinations**: We never show data that isn't functionally true.
3.  **Fail-Silent**: If we can't be 100% sure, we do nothing. We never interrupt the flow with guesses.
4.  **Artisan Control**: The user is the source of truth. The editor is the servant.

---

## 🎨 I. The Visual Contract (Perception)
*How we handle what you see.*

### Semantic Colors
We do not approximate colors. We render the exact truth of the bytecode:
- **Rule**: If it matches `0xFF...` or `[r,g,b]`, it is a color.
- **Guarantee**: We never "guess" a variable is a color unless it matches the strict structural contract.

### Asset Reconnaissance (Sprites, Audio, Fonts)
We treat assets as binary truths:
- **Cyan/Green**: The file definitively exists on disk.
- **Red**: The file definitively does not exist.
- **Rule**: We never cache "maybe" states. We check the disk logic directly.

---

## ⚡ II. The Navigation Contract (Flow)
*How we handle where you go.*

### Contextual Jumps
We do not scan your entire hard drive.
- **Rule**: Navigation (`RET`, `C-c C-o`) is context-strict. It only activates inside `require`, `read_file`, or literal asset strings.
- **Guarantee**: You will never jump to a "fuzzy match" file by accident.

---

## 📜 III. The API Contract (Autocomplete)
*How we handle what you write.*

**(Formerly "Level 5")**

This is the ultimate expression of our philosophy. It is **NOT** a Language Server (LSP). It does not parse, infer, or guess. It enforces a static agreement between you and your tools.

### Rules
1.  **Strict Definition**: You define the API structure in `dragonruby_api.yml`.
2.  **Zero Inference**: `a = args; a.` will never trigger completion. Only explicit roots (`args.`) work.
3.  **Literal Paths**: Only visible, literal paths are traversed.
4.  **Fallback Strategy**: The system looks for `dragonruby_api.yml` in the project root. If missing, it uses the global fallback contract (v0.1) bundled with the plugin.

### The Schema
We strictly respect the 2-space indented YAML contract. If it is not in the YAML, it does not exist for the editor.

### The Schema (v1)
- **Indentation**: STRICTLY 2 spaces.
- **Structure**: Maps only.
- **Roots**: Defined as a list or map keys.

**Example `dragonruby_api.yml`:**
```yaml
roots:
  - args

trees:
  args:
    state:
      tick_count: t
    outputs:
      solids: t
      sprites: t
      labels: t
```

---

## Final Statement

This project is not a collection of features. It is a cohesive system designed to preserve a **calm, predictable, and artisan workflow**.

We built this so the tool disappears, and only the creation remains.
