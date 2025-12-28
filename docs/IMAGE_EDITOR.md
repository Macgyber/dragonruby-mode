# Image Editor

DragonRuby Emacs Mode includes a built-in image editor that appears when you open any image file in a DragonRuby project.

## Requirements

**For basic features (view, rotate):** No dependencies

**For advanced features (trim, resize, compress):** [ImageMagick](https://imagemagick.org)

### Installing ImageMagick

| System | Command |
|--------|---------|
| Windows | Download from [imagemagick.org](https://imagemagick.org/script/download.php#windows) |
| macOS | `brew install imagemagick` |
| Linux | `apt install imagemagick` or `pacman -S imagemagick` |

## Toolbar Buttons

### View Controls

| Button | Function |
|--------|----------|
| `[+]` | Zoom in |
| `[-]` | Zoom out |
| `[1:1]` | Reload image (reset all transformations) |
| `[Rot]` | Rotate 90° |
| `[Info]` | Toggle metadata display |
| `[Edit]` | Open in external editor |

### ImageMagick Operations

| Button | Function |
|--------|----------|
| `[Trim]` | Remove transparent/white edges |
| `[Zip]` | Compress image |
| `[2x]` | Double image size |
| `[.5]` | Half image size |
| `[<>]` | Flip horizontal |
| `[/\]` | Flip vertical |
| `[Gry]` | Convert to grayscale |
| `[Inv]` | Invert colors |
| `[NoBG]` | Remove white background |
| `[Crop]` | Crop image (W H X Y) |
| `[Tint]` | Tint image with a specific color |
| `[PNG]` | Convert current file to PNG format |

### Safety Features

| Button | Function |
|--------|----------|
| `[Undo]` | Revert to previous state (restores backup) |

> ℹ️ **Note**: Operations create a local `.bak` file. You can press `[Undo]` to restore the original image.

## Custom External Editor

Set your preferred image editor in your `init.el`:

```elisp
;; Windows example (Graphite)
(setq dragonruby-external-image-editor "C:/Program Files/Graphite/Graphite.exe")

;; macOS example (Aseprite)
(setq dragonruby-external-image-editor "/Applications/Aseprite.app/Contents/MacOS/aseprite")
```

If not set, `[Edit]` uses your system's default application.

## Smart Source Jumping

When you click on a sprite path (e.g., `"sprites/player.png"`) in your code, Dragonruby-mode can automatically look for the *source file* instead of the compiled image.

By default, it checks for: `.aseprite`, `.graphite`, `.psd`, and `.xcf`.

**Configuration:**

```elisp
;; Add or remove extensions to prioritize
(setq dragonruby-sprite-source-extensions '(".aseprite" ".graphite" ".psd" ".xcf" ".kra"))
```

If `sprites/player.aseprite` exists (or is found in the `art/` folder), clicking `"sprites/player.png"` will open the `.aseprite` file in your system's default editor (e.g., Aseprite). If no source is found, it opens the `.png` as usual.

> 💡 **Pro Tip**: To keep your project clean, we recommend storing source files in an `art/` folder at the root of your project.
>
> **Recommended Structure:**
> - `mygame/sprites/hero.png`  (Used in code, exported asset)
> - `mygame/art/hero.aseprite` (Source file - Detected automatically!)

## Metadata Display

The header shows: `FORMAT WIDTHxHEIGHT SIZE`

Example: `PNG 32x32 1.5KB`

Toggle with `[Info]` button.
