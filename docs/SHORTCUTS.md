# 🐉 Emacs + DragonRuby Mode — Cheatsheet

> **Print this!** A quick reference for all essential keybindings.

---

## 🔤 Key Notation Guide

Emacs uses its own notation for keys. Here's how to read it:

### Modifier Keys

| Symbol | Name | Physical Key | Example |
|--------|------|--------------|---------|
| `C` | Control | `Ctrl` | `C-x` = Ctrl+x |
| `M` | Meta | `Alt` (or `Esc`) | `M-x` = Alt+x |
| `S` | Shift | `Shift` | `S-TAB` = Shift+Tab |
| `RET` | Return | `Enter` | |
| `SPC` | Space | `Spacebar` | |
| `TAB` | Tab | `Tab` | |
| `DEL` | Delete | `Backspace` | |

### Reading Combined Keys

| Notation | How to Press | Description |
|----------|--------------|-------------|
| `C-x` | Hold Ctrl, press x | Single combo |
| `M-x` | Hold Alt, press x | Single combo |
| `C-M-i` | Hold Ctrl+Alt, press i | Triple combo |
| `C-x C-f` | Ctrl+x, then Ctrl+f | Sequence (two combos) |
| `C-x b` | Ctrl+x, then just b | Sequence (combo + key) |

### Examples

| Notation | Keys to Press | What You Do |
|----------|---------------|-------------|
| `C-x C-s` | Ctrl+x, then Ctrl+s | Save file |
| `C-x C-f` | Ctrl+x, then Ctrl+f | Open file |
| `M-x` | Alt+x | Run command by name |
| `C-M-i` | Ctrl+Alt+i | Autocomplete |
| `C-g` | Ctrl+g | Cancel / Abort |
| `C-/` | Ctrl+/ | Undo |
| `M-<` | Alt+Shift+, | Go to beginning |
| `M->` | Alt+Shift+. | Go to end |

### Windows Special Notes

| Problem | Solution |
|---------|----------|
| `Alt+Tab` switches windows | Use `C-M-i` instead of `M-TAB` |
| Alt doesn't work | Press and release `Esc`, then press the key |
| Example: `M-x` alternative | Press `Esc`, release, then press `x` |

---

## Emacs Essentials

### Files
| Notation | Keys | Action |
|----------|------|--------|
| `C-x C-f` | Ctrl+x, Ctrl+f | Open file |
| `C-x C-s` | Ctrl+x, Ctrl+s | Save file |
| `C-x C-w` | Ctrl+x, Ctrl+w | Save as... |
| `C-x k` | Ctrl+x, k | Kill buffer |
| `C-x b` | Ctrl+x, b | Switch buffer |

### Navigation
| Notation | Keys | Action |
|----------|------|--------|
| `C-a` | Ctrl+a | Beginning of line |
| `C-e` | Ctrl+e | End of line |
| `M-<` | Alt+Shift+, | Beginning of file |
| `M->` | Alt+Shift+. | End of file |
| `C-s` | Ctrl+s | Search forward |
| `C-r` | Ctrl+r | Search backward |
| `M-g g` | Alt+g, g | Go to line |
| `C-l` | Ctrl+l | Center screen on cursor |

### Editing
| Notation | Keys | Action |
|----------|------|--------|
| `C-k` | Ctrl+k | Kill (cut) to end of line |
| `C-y` | Ctrl+y | Yank (paste) |
| `M-w` | Alt+w | Copy region |
| `C-w` | Ctrl+w | Cut region |
| `C-/` | Ctrl+/ | Undo |
| `C-SPC` | Ctrl+Space | Start selection |
| `C-g` | Ctrl+g | Cancel / Abort |

### Windows & Buffers
| Notation | Keys | Action |
|----------|------|--------|
| `C-x 2` | Ctrl+x, 2 | Split horizontal |
| `C-x 3` | Ctrl+x, 3 | Split vertical |
| `C-x 1` | Ctrl+x, 1 | Close other windows |
| `C-x 0` | Ctrl+x, 0 | Close this window |
| `C-x o` | Ctrl+x, o | Switch to other window |

### Help
| Notation | Keys | Action |
|----------|------|--------|
| `C-h k` | Ctrl+h, k | Describe what a key does |
| `C-h f` | Ctrl+h, f | Describe a function |
| `C-h v` | Ctrl+h, v | Describe a variable |
| `M-x` | Alt+x | Run any command by name |

---

## DragonRuby Mode

### Sprites
| Notation | Keys | Action |
|----------|------|--------|
| `C-M-i` | Ctrl+Alt+i | Autocomplete sprite path |
| `RET RET` | Enter, Enter | Confirm selection |
| `mouse-1` | Left click | Open sprite file |
| (hover) | Mouse over | Preview image tooltip |

### Navigation (Paths)
| Notation | Keys | Action |
|----------|------|--------|
| `mouse-1` | Left click | Follow require/path |
| `RET` | Enter | Open file at point |

### Mode Control
| Notation | Keys | Action |
|----------|------|--------|
| `M-x dragonruby-mode` | Alt+x, type command | Toggle mode on/off |
| `M-x eval-buffer` | Alt+x, type command | Reload current file |

---

## Image Editor (in image buffers)

When viewing an image in a DragonRuby project, a toolbar appears with these buttons:

### View Controls
| Button | Action |
|--------|--------|
| `[+]` | Zoom in |
| `[-]` | Zoom out |
| `[1:1]` | Reload image (reset) |
| `[Rot]` | Rotate 90° |
| `[Info]` | Toggle metadata display |
| `[Open]` | Open in external editor |

### ImageMagick Operations
| Button | Action |
|--------|--------|
| `[Trim]` | Remove transparent/white edges |
| `[Zip]` | Compress image |
| `[2x]` | Double size |
| `[.5]` | Half size |
| `[<>]` | Flip horizontal |
| `[/\]` | Flip vertical |
| `[Gry]` | Grayscale |
| `[Inv]` | Invert colors |
| `[NoBG]` | Remove white background |

> Requires [ImageMagick](https://imagemagick.org) for advanced operations.

---

## learnDR-mode (Future)

| Notation | Keys | Action |
|----------|------|--------|
| `C-c d` | Ctrl+c, d | Describe concept at point |
| `F1` | F1 | Help for symbol |

---

*DragonRuby Emacs Mode — v0.2.0*
