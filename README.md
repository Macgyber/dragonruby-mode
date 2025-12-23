# DragonRuby Mode for Emacs

This project follows a strict [Project Contract](CONTRACT.md).

## Philosophy
"The goal is not speed of typing, but clarity of thought."

## Installation
See [INSTALL_INSTRUCTIONS.md](INSTALL_INSTRUCTIONS.md) for setup details.

## Usage
The mode activates **automatically** when you open a Ruby file containing `def tick`. You will see `DR` in the modeline.

### Key Commands

**`M-x dragonruby-mode`**
- **Toggle manually.** Use this to force the mode ON in files that don't have a `tick` method, or to turn it OFF if you want silence.

**`M-x load-file`**
- **Manual loading.** Use this to try a specific version of the plugin without restarting Emacs (e.g., `~/emacs-packages/dragonruby-mode/src/dragonruby.el`).

## Features
- Contextual explanations of DragonRuby concepts.
- Explicit visibility of engine concepts.
