# Contributing Translations to learnDR-mode

Thank you for helping make DragonRuby education accessible worldwide! 🌍

## What We Need

We're translating **concept files** (`.org`) that explain DragonRuby concepts like `tick`, `args`, `Sprite`, etc.

## Languages in Progress

| Language | Status | Folder |
|----------|--------|--------|
| 🇺🇸 English | In progress | `docs/concepts/en/` |
| 🇪🇸 Español | In progress | `docs/concepts/es/` |
| 🇨🇳 中文 | Planned | `docs/concepts/zh/` |

## How to Contribute

### Step 1: Pick a concept to translate

Check what's available in the English folder:
```
docs/concepts/en/
├── engine/
│   ├── tick.org
│   └── args.org
├── render/
│   ├── sprite.org
│   └── label.org
...
```

### Step 2: Create the translation

1. Copy the English `.org` file
2. Create the same folder structure in your language folder
3. Translate the content

Example:
```
# Original
docs/concepts/en/engine/tick.org

# Your translation
docs/concepts/es/engine/tick.org
```

### Step 3: Translation rules

- ✅ Translate explanations and comments
- ✅ Keep code examples exactly as they are (code is universal)
- ✅ Use your language's natural phrasing
- ❌ Don't translate DragonRuby keywords (`tick`, `args`, `Sprite`)
- ❌ Don't change the `.org` structure

### Step 4: Submit

1. Fork the repository
2. Add your translated file(s)
3. Submit a Pull Request with title: `[Translation] <language>: <concept>`

Example: `[Translation] Español: tick.org`

## Template for New Concepts

```org
#+TITLE: tick — DragonRuby Entry Point
#+LANGUAGE: es
#+DESCRIPTION: Punto de entrada principal del motor

* Definición
  ...

* Ejemplo
  #+BEGIN_SRC ruby
  def tick(args)
    # Code stays in English/Ruby
  end
  #+END_SRC

* Conceptos relacionados
  - [[file:args.org][args]]
```

## Questions?

Open an issue with the `translation` label.

---

*Thank you for making DragonRuby accessible to everyone!* 🐉
