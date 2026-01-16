# Contributing to DragonRuby Emacs Mode

Thank you for your interest in contributing! This document explains how to participate effectively.

## Reporting Issues

### Yes, please report:
- Bugs that break expected behavior
- Conflicts with other modes (LSP, completion, etc.)
- Incorrect overlay rendering
- Performance problems

### When reporting:
1. Describe what you expected vs what happened
2. Include your Emacs version (`M-x emacs-version`)
3. Mention if you use Doom/Spacemacs/vanilla
4. Include a minimal reproducible example if possible

## 🚧 Developer Mode & Experimental Features

We don't hide our kitchen. Some features are shipped in the codebase but disabled by default to maintain stability. If you want to help us test, look at these "Micro-switches":

### Enabling Experimental Features

Add these to your `init.el` or `config.el` to unlock early-access tools:

| Feature Flag | Description | Status |
|--------------|-------------|--------|
| `(setq dragonruby-experimental-smart-jump t)` | **Smart Source Jump**: Opens source files (`.psd`, `.kra`) from previews. | 🧪 Experimental |
| `(setq dragonruby-experimental-concepts-guide t)` | **Living Documentation**: Symlinks code to `.org` definitions. | 🧪 Experimental |

### Your Feedback is Gold
If you enable these and find a bug or have an idea, please use the **Experimental Feedback** template when opening an issue.

## Pull Requests

### Before submitting:
- Read the [Architecture](ARCHITECTURE.md) and [Manifesto](MANIFESTO.md)
- Ensure your change aligns with the project's design philosophy

### Good PRs:
- Fix bugs without changing architecture
- Add hooks for modern Emacs (e.g., `ruby-ts-mode`)
- Improve documentation
- Add tests

### PRs that need discussion first:
- New features that add UI elements
- Changes to activation logic
- Modifications to overlay behavior

## Code Style

- Use `lexical-binding: t`
- All state must be `defvar-local` (buffer-local)
- No global hooks without cleanup
- Prefer overlays over text modification

## Testing

Run tests before submitting:
```bash
cd tests/
emacs --batch -l run-tests.el
```

## Questions?

Open an issue with the `question` label.

---

*DragonRuby Emacs Mode — v0.7.4*
