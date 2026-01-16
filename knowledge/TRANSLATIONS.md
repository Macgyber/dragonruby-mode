# Contributing Translations to learnDR-mode

Thank you for helping make DragonRuby education accessible worldwide! 🌍

## What We Need

We're translating **concept files** (`.org`) that explain DragonRuby concepts like `tick`, `args`, `Sprite`, etc.

## Languages in Progress

| Language | Status | Folder |
|----------|--------|--------|
| 🇺🇸 English | In progress | `knowledge/definitions/` |
| 🇪🇸 Español | In progress | `knowledge/definitions/` |
| 🇨🇳 中文 | Planned | `knowledge/definitions/` |
| 🇯🇵 日本語 | Planned | `knowledge/definitions/` |
| 🇫🇷 Français | Planned | `knowledge/definitions/` |
| 🇩🇪 Deutsch | Planned | `knowledge/definitions/` |

## How to Contribute

There are two ways to contribute:

### Method A: Use GitHub Issues (Easiest)
If you want to suggest a translation for a specific term or correct an existing one without dealing with git/files:
1. **[Click here to open the Translation Form on GitHub](https://github.com/Macgyber/dragonruby-mode/issues/new?template=translation_contribution.yml)**
2. Fill out the form and submit!

### Method B: Manual Translation (For .org files)

### Step 1: Pick a concept to translate

Check what's available in the English folder:
```
knowledge/definitions/
├── tick.org
├── args.org
├── sprite.org
├── label.org
...
```

### Step 2: Create the translation

1. Copy the English `.org` file
2. Create the same folder structure in your language folder
3. Translate the content

Example:
```
# Original
knowledge/definitions/tick.org

# Your translation
# (Translations currently use the same folder with separate titles/content 
# or separate folders as the project scales)
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
#+DESCRIPTION: Main engine entry point

* Definition
  ...

* Example
  #+BEGIN_SRC ruby
  def tick(args)
    # Code stays in English/Ruby
  end
  #+END_SRC

* Related concepts
  - [[file:args.org][args]]
```

## Questions?

- Open an issue using the **Translation Contribution** template.
- Reach out to the community in the DragonRuby Discord.

---

*Thank you for making DragonRuby accessible to everyone!* 🐉

---

*DragonRuby Emacs Mode — v0.7.4*
