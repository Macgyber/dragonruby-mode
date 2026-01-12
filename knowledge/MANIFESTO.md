# The DragonRuby Mode Manifesto

> *dragonruby-mode is not a tool that understands DragonRuby.*  
> *It is a tool that understands the boundary between what the editor may assist with, and what the programmer must understand.*

## I. What is Documented (Explicit)

### 1. The Philosophy
The project clearly documents what it is **NOT**:
- Not an LSP.
- Not intelligent.
- Not inferential.
- Not dynamic.

*This creates a strong conceptual boundary that protects the design.*

### 2. The Contract Model
The role of the YAML file is explicitly defined:
- It is the **sole source of truth**.
- It defines navigable structure, **not behavior**.
- It is **static** by design.

### 3. Autocompletion Rules
The governance of completion is explicit and non-negotiable:
- Roots must be **literal**.
- Paths must be **visible**.
- Inference is **forbidden**.
- Invalid contracts **disable the feature safely**.

### 4. Validation Evidence
The project includes a real game example (`validation_game.rb`), demonstrating that the system works without cheating or exceptions.

---

## II. What is NOT Documented (Intentional)

### 1. Full DragonRuby API Coverage
The project **does not** document the entire DragonRuby API.
- **Reason**: The contract is **usage-driven**, not reference-driven.

### 2. Method Semantics
The YAML **does not** explain what methods do, parameters mean, or internal behavior.
- **Reason**: That knowledge belongs to the **programmer**, not the editor.

### 3. "Correct" Game Architecture
The project **does not** teach how to structure a game.
- **Reason**: Architecture is a **human decision**, not an editor feature.

---

## III. What is Implied (But Not Written)

1.  **The Editor as an Amplifier**: The editor should amplify the programmer’s intent, not replace understanding.
2.  **Stability Over Cleverness**: Long-term stability is more valuable than short-term convenience.
3.  **Human Memory Is Limited**: Autocomplete exists to reduce **recall burden**, not to eliminate thinking.

---

## IV. The Design Resolutions

### What the Project Resolves
- Resolves **API navigation friction** without introducing magic.
- Resolves the need for **autocomplete** without lying to the user.
- Resolves **editor assistance** while preserving learning and mastery.
- Resolves **extensibility** through data, not code.

### What the Project Refuses to Resolve
- It does not resolve **"what should I write next?"**
- It does not resolve **"what does this code mean?"**
- It does not resolve **incorrect mental models**.
- It does not resolve **ignorance by automation**.

*These refusals are design decisions, not omissions.*

---

## V. Final Synthesis

**The Contract is the line.**

Everything on one side is data.  
Everything on the other side is human responsibility.

---

*Manifesto validated against Contract v0.1 and real gameplay usage.*
