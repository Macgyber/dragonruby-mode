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
| `C-M-i` | Ctrl+Alt+i | Autocomplete sprite path (inside `"sprites/"`) |
| `RET` | Enter | Open sprite in Emacs image viewer |
| `C-c RET` | Ctrl+c, Enter | Open sprite (alternative) |
| `C-c p` | Ctrl+c, p | Preview sprite in popup |
| `mouse-1` | Left click | Open sprite in Emacs image viewer |

### Paths (Smart Navigation)
| Notation | Keys | Action |
|----------|------|--------|
| **Snippets** | | |
| `req` + `C-M-i` | Type req, Ctrl+Alt+i | Expands to `require ""` |
| `reqr` + `C-M-i` | Type reqr, Ctrl+Alt+i | Expands to `require_relative ""` |
| `load` + `C-M-i` | Type load, Ctrl+Alt+i | Expands to `load ""` |
| `read` + `C-M-i` | Type read, Ctrl+Alt+i | Expands to `read_file("")` |
| `json` + `C-M-i` | Type json, Ctrl+Alt+i | Expands to `parse_json_file("")` |
| **Inside Quotes** | | |
| `C-M-i` | Ctrl+Alt+i | Open minibuffer file picker |
| `C-c o` | Ctrl+c, o | Open any project file |
| `RET` | Enter | Follow underlined path |
| `C-c RET` | Ctrl+c, Enter | Follow path (alternative) |
| `mouse-1` | Left click | Follow underlined path |

### Colors
| Notation | Keys | Action |
|----------|------|--------|
| `RET` | Enter on `■` | Open color picker |
| `C-c RET` | Ctrl+c, Enter on `■` | Open color picker (alternative) |
| `mouse-1` | Left click on `■` | Open color picker |

### Mode Control
| Notation | Keys | Action |
|----------|------|--------|
| `M-x dragonruby-mode` | Alt+x, type command | Toggle mode on/off |

### Development (Local .emacs)
| Notation | Keys | Action |
|----------|------|--------|
| `F5` | F5 | Reload local .emacs |
| `F6` | F6 | Hot reload all plugin modules |

---

### 📂 VIEW (Camera & Timeline)
| Key | Button | Action |
|-----|--------|--------|
| `<` or `,` | `<` | **Back** (Undo) |
| `S-r` (R) | `RST` | **Hard Reset** (Original) |
| `>` or `.` | `>` | **Forward** (Redo) |
| `+` | `+` | Zoom In |
| `-` | `-` | Zoom Out |
| `1:1` | `1:1` | Reset Zoom & Reload |
| `r` | `Rot` | Rotate 90° |

### 📂 TRANSFORM (Geometry)
| Key | Button | Action |
|-----|--------|--------|
| `t` | `Trim` | Remove empty margins |
| `z` | `Zip` | Compress image |
| `h` | `<>` | Flip Horizontal |
| `v` | `/\` | Flip Vertical |

### 📂 COLOR (Adjustments)
| Key | Button | Action |
|-----|--------|--------|
| `g` | `Gry` | Grayscale |
| `n` | `Inv` | Invert Colors (Negative) |

### 📂 SYSTEM (Tools & Reset)
| `i` | `Info` | Toggle Info & Debug Stage |
| `c` | `Crop` | Numerical Crop (Minibuffer) |
| `T` | `Tint` | Tint with color (Minibuffer) |
| `e` | `Edit` | **Artist Portal** (Editors + Web Links) |
| `p` | `PNG` | Convert to PNG |

> Requires [ImageMagick](https://imagemagick.org) for advanced operations.

---

## learnDR-mode (Future)

| Notation | Keys | Action |
|----------|------|--------|
| `C-c d` | Ctrl+c, d | Describe concept at point |
| `F1` | F1 | Help for symbol |

---

*DragonRuby Emacs Mode — v0.5.0*
