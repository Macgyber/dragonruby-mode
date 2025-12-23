# DragonRuby Mode for Emacs

This project follows a strict [Project Contract](docs/CONTRACT.md).

## Philosophy
"The goal is not speed of typing, but clarity of thought."

## Installation
See [INSTALL_INSTRUCTIONS.md](INSTALL_INSTRUCTIONS.md) for setup details.

## Usage
The mode activates **automatically** when you open a Ruby file containing `def tick`. You will see `DR` in the modeline.

## Commands

| Command | Description |
|---------|-------------|
| **`M-x dragonruby-mode`** | **Toggle ON/OFF.** Manually activate or deactivate the mode in the current buffer. |
| **`M-x load-file`** | **Reload Plugin.** Forces Emacs to reload the plugin code (useful for testing dev versions). |

## Features
- Contextual explanations of DragonRuby concepts.
- Explicit visibility of engine concepts.
