# Font System Guide 🅰️

## Introduction

The DragonRuby Emacs Mode Font System allows you to **visualize, validate, and manage** your game's typography without leaving Emacs. From inline thumbnails to a complete interactive viewer, everything is designed so you can make typographic decisions with confidence.

---

## 🎯 Main Features

### 1. Visual Validation in Code

When you write a font path in your code (e.g., `font: "fonts/main.ttf"`), the plugin:

- **✅ Valid font (TTF/OTF)**: Highlighted in **bright cyan** (`#00FFFF`)
- **🖼️ Inline thumbnail**: A small preview appears at the end of the line
- **✨ Context Preview**: The plugin is smart! It reads nearby `text:`, `label:`, or `string:` values and uses **that text** to render the preview instead of a generic "DragonRuby". If you write `text: "Game Over"`, the thumbnail will show those exact words in your font!
- **❌ Unsupported format (WOFF/WOFF2/EOT)**: Highlighted in **orange** with an explanatory tooltip
- **🔴 Missing file**: Highlighted in **red**

### 2. Theme-Adaptive Thumbnails

Inline previews automatically detect your Emacs theme:
- **Dark Theme**: Black background, white text
- **Light Theme**: White background, black text

The system regenerates thumbnails automatically when you change themes.

---

## 🖼️ The Interactive Font Viewer

### How to Activate

1. Place the cursor over a valid font path (highlighted in cyan)
2. Press `RET` or `C-c C-o`
3. The `*Font Viewer*` buffer will open automatically

### Viewer Interface

The viewer has **three sections** controlled by buttons in the header-line:

#### 📋 Sample (Full Sample)
Shows a long sample text to see how the font reads:
```
"The quick brown fox jumps over the lazy dragon. 
Creating games in DragonRuby is pure magic!"
```

**Button**: `[📋 Sample]`

#### 🔤 ABC (Full Alphabet)
Shows all alphabetic characters in different styles:
```
ABCDEFGHIJKLMNOPQRSTUVWXYZ
abcdefghijklmnopqrstuvwxyz
0123456789
```

**Button**: `[🔤 ABC]`

#### 🌍 Pangrams (International Pangrams)
Pangrams in various languages to test character coverage:

- **English**: "The quick brown fox jumps over the lazy dog"
- **Spanish**: "El veloz murciélago hindú comía feliz cardillo y kiwi"
- **French**: "Portez ce vieux whisky au juge blond qui fume"
- **German**: "Victor jagt zwölf Boxkämpfer quer über den großen Sylter Deich"
- **Japanese (Hiragana)**: "いろはにほへと..."

**Button**: `[🌍 Pangrams]`

---

## 🎨 Font Creative Hub

### Quick Access

Press the **`[🎨 CREATIVE]`** button in the viewer's header-line to access:

#### Predefined Web Portals
- **Google Fonts**: Massive library of free fonts
- **DaFont**: Thousands of thematic and decorative fonts
- **Font Squirrel**: Quality commercial and free fonts

#### Personal Link Management

**Add your own link**:
1. Click **`[+]`**
2. Enter the name: `"My Favorite Font"`
3. Enter the URL or local path
4. Choose a color (a random vibrant one is suggested)

**Hide or remove links**:
1. Click **`[-]`**
2. Select the link from the menu
3. Predefined ones are hidden; custom ones are deleted

#### Custom Configuration

Add your own links directly in your `config.el`:

```elisp
(setq dragonruby-user-font-links 
      '(("MyFonts" "https://www.myfonts.com/" "#FF6B9D")
        ("Adobe Fonts" "https://fonts.adobe.com/" "#FF0000")
        ("Local Collection" "~/Documents/Fonts/" "#00FF00")))
```

---

## ⚙️ Advanced Configuration

### Hide Predefined Portals

If you don't want to see some of the predefined portals:

```elisp
(setq dragonruby-hidden-creative-tools '("DaFont" "Google Fonts"))
```

### Thumbnail Cache

Previews are saved in:
- **With project**: `.dr_history/font-previews/`
- **Without project**: System temporary directory

To regenerate thumbnails, change theme or restart the mode.

---

## 🚀 Recommended Workflow

1. **Writing code**:
   - Type `font: "fonts/"`
   - Press `C-M-i` to see your available fonts
   - Select one, the thumbnail appears automatically

2. **Validating display**:
   - Press `RET` on the font
   - Review in the viewer how the text looks
   - Verify that pangrams display correctly

3. **Finding new fonts**:
   - Open the viewer
   - Press `[🎨 CREATIVE]`
   - Explore Google Fonts or DaFont
   - Download and install in `mygame/fonts/`

---

## 🐛 Troubleshooting

### Thumbnail doesn't appear
- **Check ImageMagick**: The system needs `magick` or `convert` installed
- **Check the file**: The font must exist and be TTF/OTF
- **Change theme**: This forces cache regeneration

### Viewer doesn't open
- **Check the path**: Must be a valid font (cyan)
- **Press exactly on the path**: The overlay must be present

### Unsupported format
DragonRuby only supports **TTF and OTF**. Web formats (WOFF, WOFF2, EOT) won't work in the game.

---

**DragonRuby Emacs Mode — Font System v0.7.4**
