# Image Editor 🖼️

DragonRuby Emacs Mode includes a sophisticated, professional image toolset designed for rapid asset iteration. It provides a non-destructive workflow directly within your Emacs image-view buffers.

## Requirements

**Basic Features (View, Rotate):** No external dependencies.

**Advanced Features (Trim, Resize, Compress, etc.):** Requires [ImageMagick](https://imagemagick.org/script/download.php#gsc.tab=0).

### Installing ImageMagick

| System | Command |
|--------|---------|
| Windows | Download from [imagemagick.org](https://imagemagick.org/script/download.php#windows) |
| macOS | `brew install imagemagick` |
| Linux | `apt install imagemagick` |

---

## 🌊 Fluid & Adaptive UI

The image toolbar is **responsive** and **context-aware**:
- **Accordion Logic**: Opening one tool group automatically collapses others, keeping the interface clean.
- **Dynamic Labels**: Labels automatically shrink (`VIEW` -> `V` -> 👁️) and spacings collapse to prevent UI overflow on smaller windows or split-screen views.
- **Active Context**: Group headers "light up" (background color change) when expanded so you always know which area you're working in.

---

## Tool Groups

### 📂 VIEW (Camera & Timeline)
Focuses on how you see the asset and its history.

| Key | Button | Action | Function |
|-----|--------|--------|----------|
| `<` | `<` | **Back** | Travel backward in the edit timeline. |
| `>` | `>` | **Forward**| Travel forward (Redo) in the edit timeline. |
| `+` | `+` | **Zoom In**| Scale up the visual preview. |
| `-` | `-` | **Zoom Out**| Scale down the visual preview. |
| `1:1`| `1:1`| **Reset** | Reset zoom and reload original view. |
| `r`  | `Rot`| **Rotate**| Rotate preview 90°. |

### 📂 TRANSFORM (Geometry)
Pixel-perfect scaling and flipping.

| Key | Button | Action | Function |
|-----|--------|--------|----------|
| `t` | `Trim` | **Trim** | Removes transparent/white excess margins. |
| `z` | `Zip` | **Compress**| Optimizes image size (strips metadata). |
| `2x`| `2x`   | **Double** | 2x Sharp Scale (uses `-filter point`). |
| `.5`| `.5`   | **Half**   | 0.5x Scale. |
| `h` | `<>`   | **Flip H** | Mirror horizontally. |
| `v` | `/\`   | **Flip V** | Mirror vertically. |

### 📂 COLOR (Adjustments)
Palette and tone modifications.

| Key | Button | Action | Function |
|-----|--------|--------|----------|
| `g` | `Gry`  | **Grayscale**| Convert sprite to B&W. |
| `n` | `Inv`  | **Invert**   | Invert all color channels. |
| `w` | `NBG`  | **No BG**    | Remove solid white backgrounds. |
| `T` | `Tint` | **Tint**     | Apply a color wash (Minibuffer). |

### 📂 SYSTEM (Control & Meta)
Internal state and external connections.

| Key | Button | Action | Function |
|-----|--------|--------|----------|
| `i` | `Info` | **Debug** | Toggles metadata + Dark Background. |
| `c` | `Crop` | **Crop**  | Numerical cropping (W H X Y). |
| `p` | `PNG`  | **PNG**   | Export to PNG format. |
| `R` | `RST`  | **Reset** | Hard Reset to Original version. |

---

## 🎨 CREATIVE Hub (Parent Button)

The **CREATIVE** button is your portal to web-based editors, resources, and custom tools. Click it to expand and reveal all child buttons.

### Default Web Tools

| Button | URL | Description |
|--------|-----|-------------|
| `Graphite` | editor.graphite.art | Vector design tool |
| `Piskel` | piskelapp.com | Pixel art animator |
| `DaFont` | dafont.com | Fonts & Typography |
| `Lospec` | lospec.com | Color palettes |
| `Itch` | itch.io/game-assets | Game assets |

### User Tool Management

| Button | Action |
|--------|--------|
| **[+]** | Add a custom tool (name, URL/path, random color) |
| **[-]** | Hide predefined tools OR remove your custom tools |

When adding with **[+]**:
1. Enter the tool name
2. Enter the URL or local path
3. Accept the random color or type your own (e.g., `#FF5500`)

### Customization Variables

```elisp
;; Add your own tools (name, url/path, color)
(setq dragonruby-user-creative-links
      '(("Figma" "https://figma.com" "#A259FF")
        ("Krita" "/usr/bin/krita" "#3BABFF")))

;; Hide predefined tools you don't use
(setq dragonruby-hidden-creative-tools '("Lospec" "Itch"))
```

---

## 🎞️ Non-Destructive Timeline

Every change you make (Trim, Rotate, Color) is **versioned**.
- **Snapshots**: Every edit saves a copy to a hidden `.dr_history` directory inside your project.
- **Safety**: You can navigate through *every* step of your session using the `<` and `>` buttons. 
- **Persistence**: The history is project-scoped, ensuring you can undo even after closing a session.

---

## 🛠️ Configuration

### External Editor
Add custom editors via the **[+]** button in the CREATIVE group, or set a global default:

```elisp
(setq dragonruby-external-image-editor "/usr/bin/krita")
```

### Smart Source Jumping
If enabled, clicking a sprite path in code looks for the *source* (e.g., `.kra`, `.psd`, `.xcf`) first.

```elisp
(setq dragonruby-experimental-smart-jump t)
```

> 💡 **Pro Tip**: Use the **[Info]** button to reveal "Invisible" margins before using **[Trim]**. This ensures your sprite collisions are exactly as big as the artwork they represent.

---

## 🚀 Speed Feedback: Help us make it perfect!

We want these tools to be lightning-fast and error-free. If something breaks or you have an idea for a "God-tier" feature:

- 🐞 **[Report a Bug / Tool Failure](https://github.com/Macgyber/dragonruby-mode/issues/new?template=bug_report.yml)** — *If a button doesn't react or ImageMagick fails.*
- ✨ **[Request a New Feature](https://github.com/Macgyber/dragonruby-mode/issues/new?template=feature_request.yml)** — *Tell us what tool is missing from your workflow.*
- 🧪 **[Experimental Feedback](https://github.com/Macgyber/dragonruby-mode/issues/new?template=experimental_feedback.yml)** — *Tell us how the new Creative Hub feels.*

---

*DragonRuby Emacs Mode — v0.7.2*
