# Contributing to DragonRuby Mode

## The Architecture Flow (Regla de Oro)

When adding a new feature, follow this decision tree to know where code belongs:

1.  **Is it a new Concept?** (e.g., "What is a Sprite?")
    *   👉 Go to **`src/concepts/`**.
    *   *Rule:* Define the "What", not the UI. Use pure data structures.

2.  **Is it about how to Display info?** (e.g., "Show a tooltip," "Draw an overlay")
    *   👉 Go to **`src/ui/`**.
    *   *Rule:* UI code should never know *what* a concept is, only how to read the registry.

3.  **Is it about When/Where it activates?** (e.g., "Turn on when opening .rb file," "Detect cursor move")
    *   👉 Go to **`src/mode/`**.
    *   *Rule:* The conductor of the orchestra. Connects events (Mode) to UI.

4.  **Is it Infrastructure?** (e.g., "How to store concepts," "Helper functions")
    *   👉 Go to **`src/core/`**.
    *   *Rule:* The backbone. Rarely changes.

## Adding a New Concept

1.  Create `src/concepts/your-concept.el`.
2.  Require it in `src/dragonruby.el`.
3.  Add tests in `tests/`.

## Philosophy

Remember: **"The goal is not speed of typing, but clarity of thought."**
If your change adds magic without explanation, it will be rejected.
