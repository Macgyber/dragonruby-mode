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
| `[Open]` | Open in external editor |

### ImageMagick Operations

| Button | Function | Permanent? |
|--------|----------|------------|
| `[Trim]` | Remove transparent/white edges | Yes |
| `[Zip]` | Compress image | Yes |
| `[2x]` | Double image size | Yes |
| `[.5]` | Half image size | Yes |
| `[<>]` | Flip horizontal | Yes |
| `[/\]` | Flip vertical | Yes |
| `[Gry]` | Convert to grayscale | Yes |
| `[Inv]` | Invert colors | Yes |
| `[NoBG]` | Remove white background | Yes |

> ⚠️ **Warning**: ImageMagick operations modify the file permanently!

## Custom External Editor

Set your preferred image editor in your `init.el`:

```elisp
;; Windows example
(setq dragonruby-external-image-editor "C:/Program Files/Aseprite/aseprite.exe")

;; macOS example
(setq dragonruby-external-image-editor "/Applications/Aseprite.app/Contents/MacOS/aseprite")
```

If not set, `[Open]` uses your system's default application.

## Metadata Display

The header shows: `FORMAT WIDTHxHEIGHT SIZE`

Example: `PNG 32x32 1.5KB`

Toggle with `[Info]` button.
