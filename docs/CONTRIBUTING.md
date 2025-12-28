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

## Pull Requests

### Before submitting:
- Read the [Design Philosophy](README.md#design-philosophy)
- Ensure your change aligns with the [Project Contract](README.md#project-contract)

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
