# DragonRuby Emacs Mode 🐉

A semantic-first, Zero-UI development environment for DragonRuby Game Toolkit.

[📖 Read the Technical Architecture](docs/ARCHITECTURE.md)

## 🌟 Philosophy

This mode transforms Emacs into a semantic editor for DragonRuby, utilizing **overlays** and **contextual intelligence** to verify "Does this asset exist?", "What color is this?", or "Where does this file lead?" without leaving the code.

## ✨ Features (Complete)

### 🎨 Phase 1: Semantic Colors
Automatically detects and visualizes colors in code using **Smart Hybrid Highlighting**.

*   **Arrays**: `[255, 0, 0]` and `[0, 255, 0, 128]`
*   **Hexadecimal**: `0xFF00FF`
*   **Hashes**:
    *   **One-Liners**: `{ r: 255, g: 0, b: 0 }` (Full Block)
    *   **Multiline**: `{ r: 255, ... }` (Fragmented, respects indentation)
*   **Symbols**: `:red`, `:dragon_green` (Data-Driven via `src/data/palettes.json`)
    *   🚀 **Pro Tip**: Add your own game colors to `palettes.json` for project-wide consistency!

### 🖼️ Phase 2: Semantic Sprites
Visualizes your game assets immediately.

*   **Inline Previews**: A tiny thumbnail (20px) appears right next to the filename.
*   **Rich Hover**: Hover over the path to see the full image + file size + dimensions.
*   **Validation**: Cyan = Valid, Red = Missing.
*   **Autocomplete**: Type `"sprites/` and get a list of project images.

### 🗺️ Phase 3: Universal Navigation
Turns your code into a Hypertext web.

*   **Smart Requires**: Click `require 'app/player'` to jump to the file.
*   **Data Links**: Strings like `"data/levels.json"` become clickable links.
*   **Error Detection**: Requires pointing to non-existent files are marked in Red.

## 🛠️ Installation

### Step 1: Clone the repository
```bash
git clone https://github.com/Macgyber/dragonruby-mode.git
```

### Step 2: Configure Emacs

1. **Open your init.el**:
   - Press `C-x C-f`
   - Type `~/.emacs.d/init.el`
   - Press Enter

2. **Add this code at the end**:
   ```elisp
   ;; DragonRuby Mode
   (add-to-list 'load-path "/path/to/dragonruby-mode")
   (require 'dragonruby-mode)
   
   ;; Auto-activate on .rb files
   (add-hook 'ruby-mode-hook #'dragonruby-mode)
   ;; If using tree-sitter in ruby mode, you need to use:
   (add-hook 'ruby-ts-mode-hook #'dragonruby-mode)

   ```
   
   > ⚠️ Replace `/path/to/dragonruby-mode` with your actual path!

3. **Save**: `C-x C-s`

4. **Reload**: `M-x eval-buffer` or restart Emacs

### Step 3: Verify

Open any `.rb` file. You should see:
- `🎨 DragonRuby: Loaded 28 colors.` in the minibuffer
- ` DR` indicator in the modeline

### Troubleshooting

**¿El comando no aparece después de reiniciar?** Emacs puede estar usando un archivo de configuración diferente.

#### Paso 1: Encuentra tu archivo de configuración REAL

1. Presiona `Alt` + `Shift` + `;` (esto es `M-:`)
2. Escribe: `(find-file user-init-file)`
3. Presiona `Enter`

Esto abrirá el archivo que Emacs **realmente** está usando.

#### Paso 2: Identifica cuál es

El buffer que se abre puede ser:
- `#<buffer .emacs>` → Tu archivo es `~/.emacs`
- `#<buffer init.el>` → Tu archivo es `~/.emacs.d/init.el`

> ⚠️ **Importante**: Emacs solo usa UNO de estos:
> - `~/.emacs`
> - `~/.emacs.el`  
> - `~/.emacs.d/init.el`
>
> Si editaste el incorrecto, los cambios no se cargarán.

#### Paso 3: Añade la configuración

En el archivo que se abrió, añade:

```elisp
;; DragonRuby Mode
(add-to-list 'load-path "e:/ANTIGRAVITY/dragonruby-emacs/packages/dragonruby-mode")
(require 'dragonruby-mode)
(add-hook 'ruby-mode-hook #'dragonruby-mode)
;; If using tree-sitter in ruby mode, you need to use:
(add-hook 'ruby-ts-mode-hook #'dragonruby-mode)
```

#### Paso 4: Guarda y reinicia

1. Guarda: `C-x C-s`
2. Reinicia Emacs completamente
3. Abre un archivo `.rb`

Deberías ver: `🎨 DragonRuby: Loaded 28 colors.`


## 🧪 Testing & Quality Assurance

This project uses **ERT** (Emacs Lisp Regression Testing) to ensure code quality.

### Test Coverage

| Category | Tests | Description |
|----------|-------|-------------|
| **Unit Tests** | 31 | Individual function testing |
| **Integration Tests** | 10 | Module connection verification |
| **Byte-Compilation** | 5 | All files compile without errors |
| **Performance** | 2 | Scan speed benchmarks |
| **Memory/Cleanup** | 3 | Overlay cleanup on mode disable |
| **Regression** | 2 | Known bug prevention |
| **Linting** | 2 | Code style verification |
| **Total** | **55** | ✅ All passing |

### Running Tests
```bash
cd tests/
emacs --batch -l run-tests.el
```

### 🐛 Bugs Detected & Fixed (v0.2.0)

| # | Bug | Impact | Fix |
|---|-----|--------|-----|
| 1 | **Variables globales de overlays** | Overlays se mezclaban entre buffers | `defvar-local` |
| 2 | **Función duplicada** `dragonruby--find-project-root` | Código repetido en 3 archivos | Unificada en `dragonruby-core.el` |
| 3 | **Sin debounce en `after-change-functions`** | Re-escaneo por cada tecla | Debounce 0.3s añadido |
| 4 | **Overlays no limpiados en sprites** | Memory leak al desactivar modo | `setq nil` añadido |
| 5 | **Funciones muertas** en `dragonruby-core.el` | Código no usado | Eliminadas |

> 🎯 **Early Bug Detection**: El sistema de tests detectó bugs antes de que llegaran a producción.

## 🛣️ Roadmap

*   **[ ] Phase 1.5**: Interactive Color Picker
*   **[ ] Phase 2.5**: Sprite Optimizer (ImageMagick)
*   **[ ] Phase 3.1**: Hyper-Symbol Navigation
*   **[ ] Phase 4**: Hot Reload from Emacs
*   **[ ] learnDR-mode**: Educational mode with Org-mode integration

---
*Built with ❤️ for the DragonRuby Community.*

