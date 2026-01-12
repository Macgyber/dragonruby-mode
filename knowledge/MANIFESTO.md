# The DragonRuby Mode Manifesto

> *dragonruby-mode is not a tool that understands DragonRuby.*  
> *It is a tool that understands the boundary between what the editor may assist with, and what the programmer must understand.*

## I. What is Documented (Explicit)

### 1. The Core Idea
The project is built on what it is **NOT**:
- Not an LSP.
- Not "intelligent."
- Not using inference.
- Not dynamic.

### 2. The Data Model
The role of the YAML file is simple:
- It is the **sole source of truth**.
- It defines navigation, **not behavior**.
- It is **static** by design.

### 3. Completion Rules
Completion is strictly controlled:
- Roots must be **literal**.
- Paths must be **visible**.
- Inference is **forbidden**.
- Invalid data **disables the feature safely**.

### 4. Verification
The project includes `validation_game.rb` to prove the system works reliably in a real scenario.

---

## II. What is NOT Documented (Intentional)

### 1. Full DragonRuby API Coverage
The project **does not** document the entire DragonRuby API.
- **Reason**: The model is based on **real usage**, not a library reference.

### 2. Method Semantics
The YAML **does not** explain what methods do, parameters mean, or internal behavior.
- **Reason**: That knowledge belongs to the **programmer**, not the editor.

### 3. "Correct" Game Architecture
The project **does not** teach how to structure a game.
- **Reason**: Architecture is a **human decision**, not an editor feature.

---

## III. Design Values

1.  **Amplification over Automation**: The editor should help you work faster, not think for you.
2.  **Stability over Cleverness**: A simple, predictable tool is better than a complex, "smart" one.
3.  **Human Memory**: Autocomplete is for **recalling names**, not for avoiding learning.

---

## IV. Boundaries

### What the Project Solves
- It makes **API navigation** smooth without using "magic."
- It provides **autocomplete** that is honest and predictable.
- It helps the programmer while **preserving mastery**.
- It allows **extensibility** through static data.

### What the Project Ignores
- It does not tell you **what to write next**.
- It does not explain **what your code means**.
- It doesn't fix **wrong mental models**.
- It won't replace **active learning**.

*These are intentional design choices.*

---

## V. Summary

**The data is the boundary.**

Everything on one side is static data.  
Everything on the other side is human responsibility.

---

*Manifesto verified against system v0.1 and actual usage.*
