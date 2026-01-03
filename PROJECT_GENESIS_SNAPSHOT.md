# PROJECT GENESIS SNAPSHOT
Generated on Tue Dec 30 07:47:07 -05 2025
> This document contains the complete source code and contract required to reconstruct the dragonruby-mode project.

## 1. THE CONTRACT
### docs/CONTRACT.md
```markdown
# DragonRuby Mode Contract

This document formalizes the responsibilities and interaction patterns for each module in `dragonruby-mode`. It serves as the single source of truth for the architecture, ensuring modularity and stability.

## Core Philosophy
1.  **Guiding Principle**: A module must fit entirely in your head. If you can’t explain it in one sentence, it’s too big.
2.  **Semantic First**: We provide semantic meaning (color, sprites, paths), not just syntax highlighting.
3.  **Modular Responsibility**: One module, one job. Modules should not leak responsibilities.
4.  **Interaction Pattern**: Consistent UX across all features.
5.  **Event Driven**: Modules are decoupled via `dragonruby-events`. Logic does not know about UI.

---

## Module Contracts

### 0. Core Infrastructure (`src/core/`)
**Responsibility**: The immutable foundation.
*   **Projects (`dragonruby-project.el`)**: Reliable root detection (`app/main.rb`).
*   **Assets (`dragonruby-assets.el`)**: Knowledge of file types and source relations (`.png` -> `.aseprite`).
*   **Events (`dragonruby-events.el`)**: Lightweight event bus. Interactions emit events; Actions listen.
*   **Registry (`dragonruby-registry.el`)**: The census of the world. Maps concepts to docs and overlays.

### 1. Sprite System (`src/sprites/`)
**Responsibility**: Visualize and manage image assets.
*   **Structure**:
    *   `src/sprites/dragonruby-sprite-model.el`: Pure domain truth (validation, rules).
    *   `src/sprites/dragonruby-sprite-fs.el`: Filesystem logic (finding, resolving).
    *   `src/sprites/dragonruby-sprite-overlay.el`: Visuals. Emits `sprite:click` events.
    *   `src/sprites/dragonruby-sprite-actions.el`: Listens to `sprite:click`, decides what to open.
    *   `src/sprites/dragonruby-sprite-completion.el`: CAPF logic. Uses `fs` module.
    *   `src/dragonruby-sprites.el`: Facade. Registers concept, initializes actions.
*   **Contract**:
    *   **Detection**: Must parse string literals ending in supported image extensions (.png, .jpg, etc.).
    *   **Visualization**: Display inline thumbnails or icons next to the code.
    *   **Interaction**: Click to open/preview. Prepares navigation to `docs/concepts/sprite.org`.
    *   **Completion**: `C-M-i` (CAPF) must list all project sprites.
    *   **Visuals**: Use `🖼️` icon in completion lists.

### 2. Path & Require System (`src/paths/`)
**Responsibility**: Navigate code structure and data files.
*   **Structure**: `dragonruby-path-fs`, `dragonruby-path-visuals`, `dragonruby-path-completion`.
*   **Contract**:
    *   **Detection**: Scan `require`, `load`, and generic file paths in strings.
    *   **Navigation**: Make verified paths clickable (hyperlinks).
    *   **Completion**: `C-M-i` (CAPF) smart completion based on context.
        *   `require`: Suggest `.rb` files (strip extension).
        *   `read_file`: Suggest data files (.json, .csv, .txt).
    *   **Visuals**: Use `💎` for Ruby code and `📄` for data files.

### 3. Documentation System (`dragonruby-docs.el`)
**Responsibility**: Link code concepts to knowledge.
*   **Contract**:
    *   **Source**: Read concepts solely from `docs/concepts/*.org`.
    *   **Lookup**: `M-x dragonruby-docs-visit-at-point` matches symbol at cursor to a filename AND a specific section.
    *   **Interaction**: Infer the *intent* of the jump (Definition, Usage, Error, Example).

### 4. Image Tools (`src/image-tools/`)
**Responsibility**: Manipulate specific image assets.
*   **Structure**: `dragonruby-image-modify` (Magick), `dragonruby-image-view` (Emacs UI), `dragonruby-image-ui`.
*   **Contract**:
    *   **Scope**: Active only when viewing an image file.
    *   **Tools**: Fluid & Adaptive "Header Line" toolbar (Responsive labels, Accordion menu).
    *   **Timeline**: Non-destructive navigation (< and >) with versioned snapshots in .dr_history.
    *   **Debug**: Info button toggles high-contrast background to reveal transparent margins.
    *   **Edit**: Delegates to `dragonruby-assets` to find the source file (Smart Jump).

### 5. Color System (`src/colors/`)
**Responsibility**: Visualize and Edit color usage.
*   **Structure**: `dragonruby-color-scanner` (Regex), `dragonruby-color-visuals` (Overlays), `dragonruby-color-picker` (Edit).
*   **Contract**:
    *   **Detection**: Identify RGB arrays `[r, g, b, a?]`, hashes `{r:..., a:...}`, and hex `0x...`.
    *   **Visualization**: Overlay text securely + Render interactive `■` box at end.
    *   **Transparency**: Visualize Alpha channel (dashed borders).
    *   **Interaction**: Click `■` -> Open Native Picker -> Replace code preserving original format.

### 6. Concept System (`src/concepts/`)
**Responsibility**: Semantic connections.
*   **Structure**: `dragonruby-concept-visuals`.
*   **Contract**:
    *   **Detection**: Identify keywords (`args`, `state`, `tick`).
    *   **Visualization**: Subtle interactive underline (Non-invasive).
    *   **Interaction**: Click/Enter opens `dragonruby-docs`.

---

## Semantic Integrity
1.  **The Overlay Rule**: Overlays represent DragonRuby domain concepts and are the exclusive path for the mode’s own semantic navigation. They must not depend on or interfere with LSP.
2.  **The Concept Rule**: All semantic concepts must link back to their definition in `docs/concepts/`. The editor must understand the "DragonRuby World", not just Ruby syntax.

## System Architecture

```
src/
├── core/                <-- Facade: dragonruby-core.el
├── sprites/             <-- Facade: dragonruby-sprites.el
├── paths/               <-- Facade: dragonruby-paths.el
├── image-tools/         <-- Facade: dragonruby-image-tools.el
├── colors/              <-- Facade: dragonruby-colors.el
├── concepts/            <-- Facade: dragonruby-concepts.el
├── dragonruby-mode.el   <-- Entry Point
└── dragonruby-docs.el   <-- Knowledge System
```

## Compliance
- [x] **Architecture Verified**: All modules follow Facade/Submodule pattern.
- [x] **Responsibilities Isolated**: Verified via Modular Refactor (v0.3.0).
- [x] **Contracts Signed**: 2025-12-29.
```

## 2. THE ROOT
### dragonruby-mode.el
```elisp
;;; dragonruby-mode.el --- Semantic tooling for DragonRuby -*- lexical-binding: t; -*-

;; Author: Macgyber <esteban3261g@gmail.com>
;; Version: 0.5.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: games, dragonruby, tools
;; URL: https://github.com/Macgyber/dragonruby-mode

;;; Commentary:
;; Semantic assistance for DragonRuby Game Toolkit projects.
;; Provides: color visualization, sprite previews, and path navigation.
;;
;; This is NOT an LSP. It does NOT replace Solargraph.
;; It adds visual semantics, not language intelligence.

;;; Code:

;; Setup load-path for src/ modules
;; Setup load-path for src/ modules and submodules
(eval-and-compile
  (let* ((load-file-path (cond
                          (load-in-progress load-file-name)
                          ((and (boundp 'byte-compile-current-file)
                                byte-compile-current-file)
                           byte-compile-current-file)
                          (t (buffer-file-name))))
         (src-dir (expand-file-name "src" (file-name-directory load-file-path)))
         (sprites-dir (expand-file-name "sprites" src-dir)))
    
    (add-to-list 'load-path src-dir)
    (add-to-list 'load-path (expand-file-name "core" src-dir))
    (add-to-list 'load-path (expand-file-name "colors" src-dir))
    (add-to-list 'load-path sprites-dir)
    (add-to-list 'load-path (expand-file-name "paths" src-dir))
    (add-to-list 'load-path (expand-file-name "image-tools" src-dir))
    (add-to-list 'load-path (expand-file-name "concepts" src-dir))))

;; Require modules from src/
(require 'dragonruby-core)
(require 'dragonruby-colors)
(require 'dragonruby-sprites)
(require 'dragonruby-paths)
(require 'dragonruby-image-tools)
(require 'dragonruby-docs)
(require 'dragonruby-concepts)

;; --- FEATURE FLAGS ---

(defcustom dragonruby-enable-colors t
  "Enable semantic color highlighting."
  :type 'boolean
  :group 'dragonruby)

(defcustom dragonruby-enable-sprites t
  "Enable sprite previews and autocompletion."
  :type 'boolean
  :group 'dragonruby)

(defcustom dragonruby-enable-paths t
  "Enable path navigation and require autocompletion."
  :type 'boolean
  :group 'dragonruby)

(defcustom dragonruby-enable-image-tools t
  "Enable image editing tools (zoom, rotate, etc)."
  :type 'boolean
  :group 'dragonruby)

(defcustom dragonruby-enable-docs t
  "Enable documentation lookup system."
  :type 'boolean
  :group 'dragonruby)

(defcustom dragonruby-enable-concepts t
  "Enable semantic concept highlighting."
  :type 'boolean
  :group 'dragonruby)

;; Minor mode definition
(define-minor-mode dragonruby-mode
  "Semantic assistance for DragonRuby projects."
  :lighter " DragonRuby"
  :group 'dragonruby
  (if dragonruby-mode
      (progn
        (when dragonruby-enable-colors (dragonruby-color-blocks-mode 1))
        (when dragonruby-enable-sprites (dragonruby-sprite-mode 1))
        (when dragonruby-enable-paths (dragonruby-paths-mode 1))
        (when dragonruby-enable-image-tools (dragonruby-image-tools-mode 1))
        (when dragonruby-enable-docs (dragonruby-docs-mode 1))
        (when dragonruby-enable-concepts (dragonruby-concepts-mode 1)))
    ;; Always disable everything when turning off the main mode, just to be safe/clean
    (dragonruby-color-blocks-mode -1)
    (dragonruby-sprite-mode -1)
    (dragonruby-paths-mode -1)
    (dragonruby-image-tools-mode -1)
    (dragonruby-docs-mode -1)
    (dragonruby-concepts-mode -1)))

;;;###autoload
(defun dragonruby-maybe-enable ()
  "Enable `dragonruby-mode' only if in a DragonRuby project.
A DragonRuby project is detected by the presence of `app/main.rb'
or a `.dragonruby' marker directory.

Usage in init.el:
  (add-hook \\='ruby-mode-hook #\\='dragonruby-maybe-enable)
  (add-hook \\='ruby-ts-mode-hook #\\='dragonruby-maybe-enable)"
  (when (dragonruby--find-project-root)
    (dragonruby-mode 1)))

(provide 'dragonruby-mode)
;;; dragonruby-mode.el ends here
```

## 3. THE MODULES
### src/dragonruby-concepts.el
```elisp
;;; dragonruby-concepts.el --- Concept Recognition Facade -*- lexical-binding: t; -*-

(require 'dragonruby-concept-visuals "concepts/dragonruby-concept-visuals")

(defcustom dragonruby-enable-concepts t
  "Enable semantic concept highlighting."
  :type 'boolean
  :group 'dragonruby)

(define-minor-mode dragonruby-concepts-mode
  "Semantic highlighting for DragonRuby concepts."
  :lighter " 🧠"
  (if dragonruby-concepts-mode
      (progn
        (add-hook 'after-change-functions #'dragonruby--after-concept-change nil t)
        (add-hook 'window-configuration-change-hook #'dragonruby--refresh-concepts nil t)
        (dragonruby--scan-concepts))
    (remove-hook 'after-change-functions #'dragonruby--after-concept-change t)
    (remove-hook 'window-configuration-change-hook #'dragonruby--refresh-concepts t)
    (dragonruby--clear-concept-overlays)))

(provide 'dragonruby-concepts)
```

### src/image-tools/dragonruby-image-modify.el
```elisp
;;; dragonruby-image-modify.el --- ImageMagick wrappers and Backups -*- lexical-binding: t; -*-

(require 'dragonruby-core)

(defun dragonruby--imagemagick-available-p ()
  "Check if ImageMagick is installed."
  (executable-find "magick"))

;; --- BACKUP SYSTEM ---

(defun dragonruby-image--backup (file)
  "Create a backup of FILE before editing."
  (message "backup")
  (copy-file file (concat file ".bak") t))

(defun dragonruby-image-undo ()
  "Restore the backup of the current image."
  (interactive)
  (let* ((file buffer-file-name)
         (backup (concat file ".bak")))
    (if (file-exists-p backup)
        (progn
          (copy-file backup file t)
          (revert-buffer t t t)
          (message "Reverted to backup"))
      (message "No backup found"))))

;; --- MODIFICATION COMMANDS ---

(defun dragonruby-image-trim ()
  "Trim transparent/white edges from image using ImageMagick."
  (interactive)
  (if (not (dragonruby--imagemagick-available-p))
      (message "❌ ImageMagick not found. Install from: https://imagemagick.org")
    (when buffer-file-name
      (let ((file buffer-file-name))
        (dragonruby-image--backup file)
        (shell-command (format "magick \"%s\" -trim +repage \"%s\"" file file))
        (revert-buffer t t t)
        (message "✂️ Trimmed transparent/white edges")))))

(defun dragonruby-image-compress ()
  "Compress PNG image for better performance using ImageMagick."
  (interactive)
  (if (not (dragonruby--imagemagick-available-p))
      (message "❌ ImageMagick not found. Install from: https://imagemagick.org")
    (when buffer-file-name
      (let* ((file buffer-file-name)
             (size-before (file-attribute-size (file-attributes file))))
        (dragonruby-image--backup file)
        (shell-command (format "magick \"%s\" -strip -quality 90 \"%s\"" file file))
        (let ((size-after (file-attribute-size (file-attributes file))))
          (revert-buffer t t t)
          (message "📦 Compressed: %.1f KB → %.1f KB (saved %.0f%%)"
                   (/ size-before 1024.0)
                   (/ size-after 1024.0)
                   (* 100 (- 1 (/ (float size-after) size-before)))))))))

(defun dragonruby-image-resize-2x ()
  "Double the image size (2x scale)."
  (interactive)
  (if (not (dragonruby--imagemagick-available-p))
      (message "❌ ImageMagick not found. Install from: https://imagemagick.org")
    (when buffer-file-name
      (let ((file buffer-file-name))
        (dragonruby-image--backup file)
        (shell-command (format "magick \"%s\" -resize 200%% \"%s\"" file file))
        (revert-buffer t t t)
        (message "🔍 Resized to 2x")))))

(defun dragonruby-image-resize-half ()
  "Halve the image size (0.5x scale)."
  (interactive)
  (if (not (dragonruby--imagemagick-available-p))
      (message "❌ ImageMagick not found. Install from: https://imagemagick.org")
    (when buffer-file-name
      (let ((file buffer-file-name))
        (dragonruby-image--backup file)
        (shell-command (format "magick \"%s\" -resize 50%% \"%s\"" file file))
        (revert-buffer t t t)
        (message "🔍 Resized to 0.5x")))))

(defun dragonruby-image-flip-h ()
  "Flip image horizontally (mirror)."
  (interactive)
  (if (not (dragonruby--imagemagick-available-p))
      (message "❌ ImageMagick not found. Install from: https://imagemagick.org")
    (when buffer-file-name
      (let ((file buffer-file-name))
        (shell-command (format "magick \"%s\" -flop \"%s\"" file file))
        (revert-buffer t t t)
        (message "↔️ Flipped horizontally")))))

(defun dragonruby-image-flip-v ()
  "Flip image vertically."
  (interactive)
  (if (not (dragonruby--imagemagick-available-p))
      (message "❌ ImageMagick not found. Install from: https://imagemagick.org")
    (when buffer-file-name
      (let ((file buffer-file-name))
        (shell-command (format "magick \"%s\" -flip \"%s\"" file file))
        (revert-buffer t t t)
        (message "↕️ Flipped vertically")))))

(defun dragonruby-image-grayscale ()
  "Convert image to grayscale."
  (interactive)
  (if (not (dragonruby--imagemagick-available-p))
      (message "❌ ImageMagick not found. Install from: https://imagemagick.org")
    (when buffer-file-name
      (let ((file buffer-file-name))
        (dragonruby-image--backup file)
        (shell-command (format "magick \"%s\" -colorspace Gray \"%s\"" file file))
        (revert-buffer t t t)
        (message "🎨 Converted to grayscale")))))

(defun dragonruby-image-invert ()
  "Invert image colors."
  (interactive)
  (if (not (dragonruby--imagemagick-available-p))
      (message "❌ ImageMagick not found. Install from: https://imagemagick.org")
    (when buffer-file-name
      (let ((file buffer-file-name))
        (dragonruby-image--backup file)
        (shell-command (format "magick \"%s\" -negate \"%s\"" file file))
        (revert-buffer t t t)
        (message "🔄 Colors inverted")))))

(defun dragonruby-image-remove-white-bg ()
  "Remove white background (make transparent)."
  (interactive)
  (if (not (dragonruby--imagemagick-available-p))
      (message "❌ ImageMagick not found. Install from: https://imagemagick.org")
    (when buffer-file-name
      (let ((file buffer-file-name))
        (dragonruby-image--backup file)
        (shell-command (format "magick \"%s\" -fuzz 10%% -transparent white \"%s\"" file file))
        (revert-buffer t t t)
        (message "✨ White background removed")))))

(defun dragonruby-image-crop (width height x y)
  "Crop image to WIDTHxHEIGHT+X+Y."
  (interactive "nWidth: \nnHeight: \nnX: \nnY: ")
  (if (not (dragonruby--imagemagick-available-p))
      (message "❌ ImageMagick not found.")
    (when buffer-file-name
      (let ((file buffer-file-name))
        (dragonruby-image--backup file)
        (shell-command (format "magick \"%s\" -crop %dx%d+%d+%d +repage \"%s\"" 
                               file width height x y file))
        (revert-buffer t t t)
        (message "✂️  Cropped to %dx%d at %d,%d" width height x y)))))

(defun dragonruby-image-tint (color)
  "Tint the image with COLOR."
  (interactive "sColor (name or hex): ")
  (if (not (dragonruby--imagemagick-available-p))
      (message "❌ ImageMagick not found.")
    (when buffer-file-name
      (let ((file buffer-file-name))
        (dragonruby-image--backup file)
        (shell-command (format "magick \"%s\" -fill \"%s\" -colorize 50%% \"%s\"" 
                               file color file))
        (revert-buffer t t t)
        (message "🎨 Tinted with %s" color)))))

(defun dragonruby-image-to-png ()
  "Convert current image to PNG."
  (interactive)
  (if (not (dragonruby--imagemagick-available-p))
      (message "❌ ImageMagick not found.")
    (when buffer-file-name
      (let* ((file buffer-file-name)
             (new-file (concat (file-name-sans-extension file) ".png")))
        (shell-command (format "magick \"%s\" \"%s\"" file new-file))
        (kill-buffer)
        (find-file new-file)
        (message "✨ Converted to PNG: %s" (file-name-nondirectory new-file))))))

(defun dragonruby-image-optimize ()
  "Show optimization options."
  (interactive)
  (if (not (dragonruby--imagemagick-available-p))
      (message "❌ ImageMagick not found. Install from: https://imagemagick.org")
    (message "Optimization: Use [ ✂️ Trim ] or [ 📦 Compress ] buttons")))

(provide 'dragonruby-image-modify)
```

### src/image-tools/dragonruby-image-ui.el
```elisp
;;; dragonruby-image-ui.el --- Header line and UI elements -*- lexical-binding: t; -*-

(require 'dragonruby-core)
(require 'dragonruby-image-modify)
(require 'dragonruby-image-view)

(declare-function w32-shell-execute "w32fns.c")

(defcustom dragonruby-external-image-editor nil
  "Path to your preferred image editor.
If nil, uses system default.
Examples:
  \"C:/Program Files/GIMP/gimp.exe\"
  \"C:/Program Files/Aseprite/aseprite.exe\"
  \"/Applications/Aseprite.app/Contents/MacOS/aseprite\""
  :type '(choice (const :tag "System default" nil)
                 (file :tag "Custom editor path"))
  :group 'dragonruby)

(defvar-local dragonruby--show-image-info t
  "Whether to show image info in header-line.")

(defun dragonruby--make-header-button (label action help &optional face)
  "Create a clickable button string for header-line."
  (propertize (concat " " label " ")
              'face (append `(:box (:line-width 2 :style released-button)
                              :background "#222222" :foreground "white"
                              ,@face))
              'mouse-face '(:box (:line-width 2 :style pressed-button) :background "#444444" :foreground "white")
              'help-echo help
              'local-map (let ((map (make-sparse-keymap)))
                          (define-key map [header-line mouse-1] action)
                          map)))

(defun dragonruby--get-image-info-string ()
  "Get image metadata as a formatted string for header-line."
  (when buffer-file-name
    (condition-case nil
        (let* ((attrs (file-attributes buffer-file-name))
               (size (file-attribute-size attrs))
               (size-str (cond
                          ((> size 1048576) (format "%.1fMB" (/ size 1048576.0)))
                          ((> size 1024) (format "%.1fKB" (/ size 1024.0)))
                          (t (format "%dB" size))))
               (ext (upcase (or (file-name-extension buffer-file-name) "?")))
               (img (create-image buffer-file-name))
               (img-size (image-size img t))
               (width (truncate (car img-size)))
               (height (truncate (cdr img-size))))
          (propertize (format "%s %dx%d %s" ext width height size-str)
                      'face '(:weight bold)))
      (error ""))))

(defun dragonruby-image-info ()
  "Toggle image info display in header-line."
  (interactive)
  (setq dragonruby--show-image-info (not dragonruby--show-image-info))
  (dragonruby--setup-image-header-line)
  (message "Image info: %s" (if dragonruby--show-image-info "ON" "OFF")))

(defun dragonruby-image-open-external ()
  "Open image (or its source file) in external editor."
  (interactive)
  (when buffer-file-name
    (let* ((source (and (bound-and-true-p dragonruby-experimental-smart-jump)
                        (dragonruby--find-source-file buffer-file-name)))
           (target-file (or source buffer-file-name))
           (editor dragonruby-external-image-editor))
      
      (when source
        (message "Found source file: %s" (file-name-nondirectory source)))

      (if editor
          (progn
            (start-process "dr-editor" nil editor target-file)
            (message "Opened in: %s" (file-name-nondirectory editor)))
        (cond
         ((eq system-type 'windows-nt)
          (w32-shell-execute "open" target-file))
         ((eq system-type 'darwin)
          (start-process "open" nil "open" target-file))
         (t
          (start-process "xdg-open" nil "xdg-open" target-file)))
        (message "Opened in system default editor")))))

(defun dragonruby--setup-image-header-line ()
  "Set up header-line with image editing buttons."
  (setq header-line-format
        (list
         (dragonruby--make-header-button "+" #'dragonruby-image-zoom-in "Zoom In")
         " "
         (dragonruby--make-header-button "-" #'dragonruby-image-zoom-out "Zoom Out")
         " "
         (dragonruby--make-header-button "1:1" #'dragonruby-image-reset-zoom "Reset")
         " "
         (dragonruby--make-header-button "Rot" #'dragonruby-image-rotate "Rotate")
         " "
         (dragonruby--make-header-button "Info" #'dragonruby-image-info "Info")
         " | "
         (dragonruby--make-header-button "Trim" #'dragonruby-image-trim "Trim edges" '(:foreground "orange"))
         " "
         (dragonruby--make-header-button "Zip" #'dragonruby-image-compress "Compress" '(:foreground "green"))
         " "
         (dragonruby--make-header-button "2x" #'dragonruby-image-resize-2x "Double" '(:foreground "cyan"))
         " "
         (dragonruby--make-header-button ".5" #'dragonruby-image-resize-half "Half" '(:foreground "cyan"))
         " "
         (dragonruby--make-header-button "<>" #'dragonruby-image-flip-h "Flip H")
         " "
         (dragonruby--make-header-button "/\\" #'dragonruby-image-flip-v "Flip V")
         " "
         (dragonruby--make-header-button "Gry" #'dragonruby-image-grayscale "Grayscale" '(:foreground "gray"))
         " "
         (dragonruby--make-header-button "Inv" #'dragonruby-image-invert "Invert" '(:foreground "magenta"))
         " "
         (dragonruby--make-header-button "NoBG" #'dragonruby-image-remove-white-bg "Remove BG" '(:foreground "pink"))
         " | "
         (dragonruby--make-header-button "Crop" #'dragonruby-image-crop "Crop (W H X Y)" '(:foreground "yellow"))
         " "
         (dragonruby--make-header-button "Tint" #'dragonruby-image-tint "Tint Color" '(:foreground "yellow"))
         " "
         (dragonruby--make-header-button "PNG" #'dragonruby-image-to-png "Convert to PNG" '(:foreground "yellow"))
         " | "
         (dragonruby--make-header-button "Undo" #'dragonruby-image-undo "Revert to backup" '(:foreground "red" :weight bold))
         " | "
         (dragonruby--make-header-button "Edit" #'dragonruby-image-open-external "External Editor")
         '(:eval (if dragonruby--show-image-info
                     (concat "  " (or (dragonruby--get-image-info-string) ""))
                   "")))))

(provide 'dragonruby-image-ui)
```

### src/image-tools/dragonruby-image-view.el
```elisp
;;; dragonruby-image-view.el --- Emacs buffer image viewing controls -*- lexical-binding: t; -*-

(require 'image-mode)

(defun dragonruby--goto-image ()
  "Move point to the first image in the buffer."
  (goto-char (point-min))
  (while (and (not (get-text-property (point) 'display))
              (not (eobp)))
    (forward-char 1)))

(defvar-local dragonruby--image-scale 1.0
  "Current scale factor for image zoom.")

(defun dragonruby-image-zoom-in ()
  "Zoom in the image."
  (interactive)
  (condition-case _err
      (progn
        (setq dragonruby--image-scale (* dragonruby--image-scale 1.2))
        (image-transform-set-scale dragonruby--image-scale))
    (error (message "⚠️ Zoom not available for this image type"))))

(defun dragonruby-image-zoom-out ()
  "Zoom out the image."
  (interactive)
  (condition-case _err
      (progn
        (setq dragonruby--image-scale (/ dragonruby--image-scale 1.2))
        (image-transform-set-scale dragonruby--image-scale))
    (error (message "⚠️ Zoom not available for this image type"))))

(defun dragonruby-image-rotate ()
  "Rotate the image 90 degrees."
  (interactive)
  (dragonruby--goto-image)
  (when (get-text-property (point) 'display)
    (image-rotate)))

(defun dragonruby-image-reset-zoom ()
  "Reset image by reloading the file completely."
  (interactive)
  (let ((file buffer-file-name))
    (when file
      (kill-buffer)
      (find-file file)
      (message "Image reloaded"))))

(provide 'dragonruby-image-view)
```

### src/core/dragonruby-assets.el
```elisp
;;; dragonruby-assets.el --- Asset definitions and source finding w/ Smart Jump -*- lexical-binding: t; -*-

(require 'dragonruby-project)

(defconst dragonruby-asset-dirs
  '((sprite . "sprites")
    (background . "sprites")
    (audio . "audio")
    (code . "app")))

(defconst dragonruby-image-exts '("png" "bmp" "jpg" "jpeg"))
(defconst dragonruby-audio-exts '("wav" "ogg" "mp3"))
(defconst dragonruby-code-exts  '("rb"))

(defcustom dragonruby-sprite-source-extensions '(".aseprite" ".ase" ".psd" ".xcf" ".graphite")
  "List of source extensions to prioritize when opening a sprite."
  :type '(repeat string)
  :group 'dragonruby)

(defcustom dragonruby-experimental-smart-jump nil
  "Enable experimental Smart Source Jumping.
If non-nil, clicking a sprite or [Edit] opens the source file (e.g. .aseprite)
if found in the same directory or \\='art/\\=' folder."
  :type 'boolean
  :group 'dragonruby)

(defun dragonruby--find-source-file (path)
  "Find a source file (e.g. .aseprite) for the given image PATH.
Checks:
1. The same directory as PATH.
2. The \\='art/\\=' directory at project root."
  (let ((base-name (file-name-sans-extension path))
        (extensions dragonruby-sprite-source-extensions)
        (root (dragonruby--find-project-root))
        (found nil))
    
    ;; 1. Check local directory
    (dolist (ext extensions)
      (let ((source-path (concat base-name ext)))
        (when (and (not found) (file-exists-p source-path))
          (setq found source-path))))
    
    ;; 2. Check 'art/' folder if not found locally
    (when (and (not found) root)
      (let* ((rel-name (file-name-nondirectory base-name))
             (art-dir (expand-file-name "art" root)))
        (when (file-directory-p art-dir)
          (dolist (ext extensions)
            (let ((source-path (expand-file-name (concat rel-name ext) art-dir)))
              (when (and (not found) (file-exists-p source-path))
                (setq found source-path)))))))
    found))

(provide 'dragonruby-assets)
```

### src/core/dragonruby-utils.el
```elisp
;;; dragonruby-utils.el --- Generic utility functions -*- lexical-binding: t; -*-

(defun dragonruby--files-in (dir extensions)
  "List files recursively in DIR matching EXTENSIONS."
  (when (and dir (file-directory-p dir))
    (directory-files-recursively
     dir
     (concat "\\." (regexp-opt extensions) "$"))))

;; --- DEBOUNCE UTILITY ---
(defvar-local dragonruby--debounce-timer nil
  "Timer for debouncing scan operations.")

(defun dragonruby--debounce (func delay)
  "Run FUNC after DELAY seconds, canceling any pending call."
  (when dragonruby--debounce-timer
    (cancel-timer dragonruby--debounce-timer))
  (setq dragonruby--debounce-timer
        (run-with-idle-timer delay nil func)))

(provide 'dragonruby-utils)
```

### src/core/dragonruby-project.el
```elisp
;;; dragonruby-project.el --- Project detection and root finding -*- lexical-binding: t; -*-

(defun dragonruby--find-project-root ()
  "Find the root of the DragonRuby project.
Looks for app/main.rb, dragonruby executable, or .dragonruby/ folder."
  (let ((dir (file-name-directory (or buffer-file-name default-directory))))
    (or (locate-dominating-file dir "app/main.rb")
        (locate-dominating-file dir "dragonruby")
        (locate-dominating-file dir ".dragonruby/")
        (locate-dominating-file dir "app")
        dir)))

;; Alias for backward compatibility
(defalias 'dragonruby--project-root 'dragonruby--find-project-root)

(provide 'dragonruby-project)
```

### src/paths/dragonruby-path-visuals.el
```elisp
;;; dragonruby-path-visuals.el --- Overlays and visuals for paths -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'dragonruby-path-fs)

(defvar-local dragonruby--path-overlays nil
  "List of path overlays in the current buffer.")

(defun dragonruby--clear-path-overlays ()
  (mapc #'delete-overlay dragonruby--path-overlays)
  (setq dragonruby--path-overlays nil))

(defun dragonruby--make-path-overlay (start end _text real-path type valid)
  (let* ((is-error (and (eq type 'ruby) (not valid)))
         (face-props (if valid 
                         '(:foreground "#61AFEF" :underline t)
                       (if is-error '(:foreground "#E06C75" :underline (:style wave :color "#E06C75")) nil)))
         (help (if valid (format "Jump to: %s" (file-relative-name real-path))
                 (if is-error "❌ Ruby file not found" nil))))
    
    (when face-props
      (let ((ov (make-overlay start end)))
        (overlay-put ov 'face face-props)
        (overlay-put ov 'help-echo help)
        (overlay-put ov 'dragonruby-path t)
        (when valid
          (overlay-put ov 'keymap 
                       (let ((map (make-sparse-keymap)))
                         (define-key map [mouse-1] (lambda () (interactive) (find-file real-path)))
                         map))
          (overlay-put ov 'mouse-face 'highlight))
        (push ov dragonruby--path-overlays)))))

(defun dragonruby--scan-paths ()
  (dragonruby--clear-path-overlays)
  (save-excursion
    (goto-char (point-min))
    ;; 1. RUBY REQUIRES
    (while (re-search-forward "\\(?:require\\|require_relative\\|load\\)\\s-*[( ]\\s-*[\"']\\([^\"']+\\)[\"']" nil t)
      (let* ((raw-path (match-string 1))
             (start (match-beginning 1))
             (end (match-end 1))
             (abs-path (dragonruby--resolve-path raw-path 'ruby))
             (exists (file-exists-p abs-path)))
        (when (or exists (string-prefix-p "app/" raw-path) (string-match-p "/" raw-path))
          (dragonruby--make-path-overlay start end raw-path abs-path 'ruby exists))))

    ;; 2. DATA FILES
    (goto-char (point-min))
    (while (re-search-forward "\"\\([^\"\n]+\\)\"" nil t)
      (let* ((raw-path (match-string 1))
             (start (match-beginning 1))
             (end (match-end 1))
             (ext (file-name-extension raw-path)))
        (when (and ext (member (downcase ext) dragonruby-data-extensions))
          (let* ((abs-path (dragonruby--resolve-path raw-path 'data))
                 (exists (file-exists-p abs-path)))
            (when exists
              (dragonruby--make-path-overlay start end raw-path abs-path 'data t))))))))

(defun dragonruby--after-path-change (_beg _end _len)
  "Debounced path scanning after buffer change."
  (dragonruby--debounce #'dragonruby--scan-paths 0.3))

(defun dragonruby--refresh-paths ()
  "Refresh path overlays when buffer becomes visible."
  (when (and (bound-and-true-p dragonruby-paths-mode)
             (eq (current-buffer) (window-buffer)))
    (dragonruby--scan-paths)))

(provide 'dragonruby-path-visuals)
```

### src/paths/dragonruby-path-fs.el
```elisp
;;; dragonruby-path-fs.el --- File system operations for paths -*- lexical-binding: t; -*-

(require 'dragonruby-core)

(defcustom dragonruby-data-extensions '("json" "txt" "csv" "tsv" "xml" "yml" "yaml")
  "List of data file extensions to support for autocomplete and lookup."
  :type '(repeat string)
  :group 'dragonruby)

(defun dragonruby--resolve-path (raw-path type)
  (let* ((root (dragonruby--find-project-root))
         (candidate (if (and (eq type 'ruby) (not (string-suffix-p ".rb" raw-path)))
                        (concat raw-path ".rb")
                      raw-path)))
    (expand-file-name candidate root)))

(defun dragonruby--get-all-ruby-files ()
  "Recursively find all .rb files."
  (let* ((root (dragonruby--find-project-root))
         (files (directory-files-recursively root "\\.rb$")))
    (mapcar (lambda (f) 
              (file-name-sans-extension (file-relative-name f root))) 
            files)))

(defun dragonruby--get-all-data-files ()
  "Recursively find all supported data files (json, txt, csv, etc)."
  (let* ((root (dragonruby--find-project-root))
         (exts-regexp (concat "\\.\\(" (regexp-opt dragonruby-data-extensions) "\\)$"))
         (files (directory-files-recursively root exts-regexp)))
    (mapcar (lambda (f) (file-relative-name f root)) files)))

(provide 'dragonruby-path-fs)
```

### src/paths/dragonruby-path-completion.el
```elisp
;;; dragonruby-path-completion.el --- Autocomplete logic for paths -*- lexical-binding: t; -*-

(require 'dragonruby-path-fs)

(defun dragonruby--annotate-candidate (candidate)
  "Return icon/annotation for completion candidate."
  (if (or (string-match-p "\\.rb$" candidate)
          (not (string-match-p "\\." candidate))) ;; No ext usually means ruby in require
      " 💎 Ruby"
    " 📄 Data"))

(defun dragonruby--make-completion-table (candidates)
  "Create a completion table with annotations."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (annotation-function . ,#'dragonruby--annotate-candidate))
      (complete-with-action action candidates string pred))))

(defun dragonruby-path-completion-at-point ()
  "CAPF backend for Ruby Requires, \\='req\\=' expansion, and Data file paths."
  (let ((state (syntax-ppss)))
    (cond
     ;; 1. Inside a String? -> Path Completion
     ((nth 3 state)
      (save-excursion
        (goto-char (nth 8 state)) ;; Start of string
        (backward-sexp 1) ;; Previous symbol
        (let* ((start (1+ (nth 8 state)))
               (end (point))
               (candidates (append (dragonruby--get-all-ruby-files)
                                   (dragonruby--get-all-data-files))))
          (list start end
                (dragonruby--make-completion-table candidates)
                :exclusive 'no))))

     ;; 2. At 'req' symbol? -> Expand to require '...'
     ((looking-back "\\breq" (- (point) 3))
      (let* ((start (- (point) 3))
             (end (point))
             (candidates (dragonruby--get-all-ruby-files)))
        (list start end
              (dragonruby--make-completion-table candidates)
              :exclusive 'no
              :exit-function
              (lambda (str _status)
                (delete-region start end) ;; Remove 'req'
                (delete-char (- (length str))) ;; Remove inserted candidate
                (insert (format "require '%s'" str))))))

     ;; 3. Generic Symbol (start of path)? -> Path Completion
     (t nil))))

(defun dragonruby--setup-path-capf ()
  (add-hook 'completion-at-point-functions #'dragonruby-path-completion-at-point nil t))

(defun dragonruby--teardown-path-capf ()
    (remove-hook 'completion-at-point-functions #'dragonruby-path-completion-at-point t))

(provide 'dragonruby-path-completion)
```

### src/dragonruby-colors.el
```elisp
;;; dragonruby-colors.el --- Color Facade -*- lexical-binding: t; -*-

(require 'dragonruby-core)
(require 'dragonruby-color-utils "colors/dragonruby-color-utils")
(require 'dragonruby-color-visuals "colors/dragonruby-color-visuals")
(require 'dragonruby-color-scanner "colors/dragonruby-color-scanner")
(require 'dragonruby-color-picker "colors/dragonruby-color-picker")

(defun dragonruby--after-color-change (_beg _end _len)
  "Debounced color scanning after buffer change."
  (dragonruby--debounce #'dragonruby--scan-colors 0.3))

(defun dragonruby--refresh-colors ()
  "Refresh color overlays when buffer becomes visible."
  (when (and (bound-and-true-p dragonruby-color-blocks-mode)
             (eq (current-buffer) (window-buffer)))
    (dragonruby--scan-colors)))

(define-minor-mode dragonruby-color-blocks-mode
  "Smart color highlighting."
  :lighter ""
  (if dragonruby-color-blocks-mode
      (progn
        (add-hook 'after-change-functions #'dragonruby--after-color-change nil t)
        (add-hook 'window-configuration-change-hook #'dragonruby--refresh-colors nil t)
        (dragonruby--scan-colors))
    (remove-hook 'after-change-functions #'dragonruby--after-color-change t)
    (remove-hook 'window-configuration-change-hook #'dragonruby--refresh-colors t)
    (dragonruby--clear-color-overlays)))

(provide 'dragonruby-colors)
```

### src/dragonruby-docs.el
```elisp
;;; dragonruby-docs.el --- Documentation linkage and lookup -*- lexical-binding: t; -*-

(require 'dragonruby-core)
(require 'org)

(defvar dragonruby-docs-directory
  (let ((current-dir (file-name-directory (or load-file-name buffer-file-name))))
    (expand-file-name "../docs/concepts" current-dir))
  "Directory containing local DragonRuby documentation concepts.")

;; --- FEATURE FLAGS ---

(defcustom dragonruby-experimental-concepts-docs nil
  "Enable experimental concept documentation lookup system via .org files.
Refers to the \\='Living Documentation\\=' system."
  :type 'boolean
  :group 'dragonruby)

;; --- REGISTRY PARSING ---

(defvar dragonruby-docs--concept-map nil
  "Hash table mapping search terms to docs filenames, loaded from index.org.")

(defun dragonruby-docs--parse-registry ()
  "Parse docs/concepts/index.org and populate `dragonruby-docs--concept-map`."
  (let ((index-file (expand-file-name "index.org" dragonruby-docs-directory))
        (map (make-hash-table :test 'equal)))
    (when (file-exists-p index-file)
      (with-temp-buffer
        (insert-file-contents index-file)
        (goto-char (point-min))
        (while (re-search-forward "^\\* \\(.*\\)" nil t)
          (let ((_current-concept (match-string 1)))
            ;; Find properties block
            (when (re-search-forward ":PROPERTIES:" nil t)
              (let ((doc-file nil)
                    (terms nil))
                (save-excursion
                  (when (re-search-forward ":DOC: \\(.*\\)" nil t)
                    (setq doc-file (match-string 1)))
                  (goto-char (match-beginning 0)) ;; Back to properties
                  (when (re-search-forward ":SEARCH_TERMS: \\(.*\\)" nil t)
                    (setq terms (split-string (match-string 1) " " t))))
                
                (when (and doc-file terms)
                  (dolist (term terms)
                    (puthash term doc-file map)))))))))
    (setq dragonruby-docs--concept-map map)))

(defun dragonruby-docs--find-concept-for-symbol (symbol)
  "Find the documentation file for SYMBOL using the registry map."
  (unless dragonruby-docs--concept-map
    (dragonruby-docs--parse-registry))
  (gethash symbol dragonruby-docs--concept-map))

(defun dragonruby-docs-open (filename &optional anchor)
  "Open documentation FILENAME (e.g. sprite.org).
If ANCHOR is provided, it is treated as a semantic intent (e.g. \\='definition\\=',
\\='example\\='). The function searches for a :NAV-TARGET: property matching ANCHOR.
Falls back to searching for a header matching ANCHOR title if no property found."
  (let ((path (expand-file-name filename dragonruby-docs-directory)))
    (if (file-exists-p path)
        (progn
          (find-file path)
          (when anchor
            (goto-char (point-min))
            ;; 1. Try Semantic Property Search (:NAV-TARGET: anchor)
            (let ((target-found nil))
              (while (and (not target-found) (re-search-forward (format ":NAV-TARGET: +%s" (regexp-quote anchor)) nil t))
                (save-excursion
                  (if (re-search-backward "^\\*+ " nil t)
                      (setq target-found (point))
                    (setq target-found nil))))
              
              (if target-found
                  (progn
                    (goto-char target-found)
                    (recenter-top-bottom 0)
                    (if (fboundp 'org-fold-show-entry)
                        (org-fold-show-entry)
                      (with-no-warnings (org-show-entry))))
                
                ;; 2. Fallback: Search for Header Title exact match
                (goto-char (point-min))
                (if (re-search-forward (format "^\\*+ %s" (regexp-quote anchor)) nil t)
                    (progn
                      (recenter-top-bottom 0)
                      (if (fboundp 'org-fold-show-entry)
                          (org-fold-show-entry)
                        (with-no-warnings (org-show-entry))))
                  (message "⚠️ Section for intention '%s' not found in %s" anchor filename)))))
          (message "📖 Opened knowledge node: %s" filename))
      (message "❌ Documentation file missing: %s" filename))))

(defun dragonruby-docs-visit-at-point ()
  "Intelligently open documentation for the concept at point.
Queries the `index.org` registry to match symbols to concepts.
Defaults to showing the \\='definition\\=' section."
  (interactive)
  (if (not dragonruby-experimental-concepts-docs)
      (message "🔒 Concept documentation is currently disabled (Experimental). Set `dragonruby-experimental-concepts-docs` to t to enable.")
    (let* ((symbol (thing-at-point 'symbol t))
           (concept-file (when symbol (dragonruby-docs--find-concept-for-symbol symbol))))
      
      (if concept-file
          (dragonruby-docs-open concept-file "definition")
        ;; Fallback: Try exact filename match or show all concepts
        (let* ((all-concepts (when dragonruby-docs--concept-map (hash-table-keys dragonruby-docs--concept-map)))
               (selection (completing-read (format "No exact match for '%s'. Search concept: " (or symbol "")) 
                                           all-concepts)))
          (when selection
             (dragonruby-docs-open (gethash selection dragonruby-docs--concept-map) "definition")))))))

(define-minor-mode dragonruby-docs-mode
  "Minor mode to enable documentation lookups."
  :lighter ""
  :group 'dragonruby
  ;; Keymaps or setup could go here
  )

(provide 'dragonruby-docs)
;;; dragonruby-docs.el ends here
```

### src/dragonruby-core.el
```elisp
;;; dragonruby-core.el --- Core Facade for DragonRuby Mode -*- lexical-binding: t; -*-

(require 'dragonruby-project "core/dragonruby-project")
(require 'dragonruby-utils "core/dragonruby-utils")
(require 'dragonruby-assets "core/dragonruby-assets")

(provide 'dragonruby-core)
```

### src/dragonruby-paths.el
```elisp
;;; dragonruby-paths.el --- Universal Code & Data Navigation Facade -*- lexical-binding: t; -*-

(require 'dragonruby-path-fs "paths/dragonruby-path-fs")
(require 'dragonruby-path-visuals "paths/dragonruby-path-visuals")
(require 'dragonruby-path-completion "paths/dragonruby-path-completion")

(define-minor-mode dragonruby-paths-mode
  "Universal Navigation."
  :lighter ""
  (if dragonruby-paths-mode
      (progn
        (add-hook 'after-change-functions #'dragonruby--after-path-change nil t)
        (add-hook 'window-configuration-change-hook #'dragonruby--refresh-paths nil t)
        (dragonruby--setup-path-capf)
        (dragonruby--scan-paths))
    (remove-hook 'after-change-functions #'dragonruby--after-path-change t)
    (remove-hook 'window-configuration-change-hook #'dragonruby--refresh-paths t)
    (dragonruby--teardown-path-capf)
    (dragonruby--clear-path-overlays)))

(provide 'dragonruby-paths)
```

### src/colors/dragonruby-color-visuals.el
```elisp
;;; dragonruby-color-visuals.el --- Overlays for color visualization -*- lexical-binding: t; -*-

(require 'dragonruby-color-utils)
(require 'dragonruby-color-picker)

(defvar-local dragonruby--color-overlays nil
  "List of color overlays in the current buffer.")

(defun dragonruby--clear-color-overlays ()
  (mapc #'delete-overlay dragonruby--color-overlays)
  (setq dragonruby--color-overlays nil))

(defun dragonruby--make-color-overlay (start end r g b &optional alpha format)
  "Create overlays: Text highlight + Clickable Color Box at end.
FORMAT: \\='array, \\='hex, or \\='hash."
  (let* ((hex (dragonruby--rgb-to-hex r g b))
         (is-transparent (and alpha (< alpha 255)))
         
         ;; 1. TEXT OVERLAY: Subtle highlight, no background bomb
         (text-ov (make-overlay start end)))
    
    (overlay-put text-ov 'face `(:underline (:color ,hex :style line)))
    (overlay-put text-ov 'dragonruby-color t)
    (push text-ov dragonruby--color-overlays)

    ;; 2. BOX OVERLAY: The clickable swatch at the end
    (let* ((box-ov (make-overlay end end)) ;; Zero-width overlay at end position
           (box-keymap (make-sparse-keymap))
           (box-str (propertize " ■" 
                               'face `(:foreground ,hex :height 1.2
                                       ,@(if is-transparent '(:box (:line-width -1 :style dashed :color "white")) nil))
                               'mouse-face 'highlight
                               'help-echo (format "Edit Color (%s)" hex))))
      
      (define-key box-keymap [mouse-1] 
        (lambda () (interactive) (dragonruby-color-picker-edit start end format)))
      
      (overlay-put box-ov 'after-string box-str)
      (overlay-put box-ov 'keymap box-keymap)
      (overlay-put box-ov 'dragonruby-color t)
      (push box-ov dragonruby--color-overlays))))

(defun dragonruby--make-simple-overlay (start end hex)
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'face `(:background ,hex :box (:line-width -1 :color "grey50")))
    (overlay-put ov 'dragonruby-color t)
    (push ov dragonruby--color-overlays)))

(provide 'dragonruby-color-visuals)
```

### src/colors/dragonruby-color-picker.el
```elisp
;;; dragonruby-color-picker.el --- Functional Color Picker -*- lexical-binding: t; -*-

(require 'dragonruby-color-utils)

(defun dragonruby--hex-to-rgb (hex)
  "Convert #RRGGBB to list (r g b)."
  (let ((r (string-to-number (substring hex 1 3) 16))
        (g (string-to-number (substring hex 3 5) 16))
        (b (string-to-number (substring hex 5 7) 16)))
    (list r g b)))

(defun dragonruby-color-picker-edit (start end format)
  "Open color picker and replace text at START-END based on FORMAT."
  (interactive)
  (let* ((current-text (buffer-substring-no-properties start end))
         ;; Using native read-color for now
         (new-color-name (read-color (format "Choose color (replacing %s): " current-text)))
         ;; Convert whatever emacs gives us (name or hex) to canonical Hex #RRGGBB
         (new-hex (if (string-prefix-p "#" new-color-name)
                      new-color-name
                    (apply #'format "#%02x%02x%02x" (color-values new-color-name))))
         (rgb (dragonruby--hex-to-rgb new-hex))
         (r (nth 0 rgb))
         (g (nth 1 rgb))
         (b (nth 2 rgb))
         
         ;; Construct replacement string based on format
         (replacement 
          (cond
           ((eq format 'array)
            (format "[%d, %d, %d]" r g b))
           
           ((eq format 'hex)
            (format "0x%02X%02X%02X" r g b)) ;; Keep 0x prefix standard
           
           ((eq format 'hash)
            (format "{r: %d, g: %d, b: %d}" r g b))
           
           (t current-text)))) ;; Fallback
    
    (when (not (string= new-color-name ""))
      (delete-region start end)
      (insert replacement)
      (message "🎨 Updated color to %s" replacement))))

(provide 'dragonruby-color-picker)
```

### src/colors/dragonruby-color-scanner.el
```elisp
;;; dragonruby-color-scanner.el --- Regex patterns to find colors in code -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'dragonruby-color-visuals)

(defun dragonruby--process-hash-match (match-start limit)
  "Helper to process a potential hash starting at MATCH-START."
  ;; Determine context (one-line vs multiline)
  (let* ((matches '())
          (r nil) (g nil) (b nil) (a nil))

    (save-excursion
      (goto-char match-start)
      ;; Search for r, g, b, a and optional comma
      (while (re-search-forward "\\b\\([rgba]\\):?\\s-*\\([0-9]+\\)\\s-*\\(,\\)?" limit t)
        (push (list (match-beginning 0) (match-end 0) 
                    (match-string 1) 
                    (string-to-number (match-string 2))) 
              matches))
      
      ;; Verify we have all three components
      (dolist (m matches)
        (cond
         ((string= (nth 2 m) "r") (setq r (nth 3 m)))
         ((string= (nth 2 m) "g") (setq g (nth 3 m)))
         ((string= (nth 2 m) "b") (setq b (nth 3 m)))
         ((string= (nth 2 m) "a") (setq a (nth 3 m)))))
      
      (when (and r g b)
        (let ((start (apply #'min (mapcar #'car matches)))
              (end (apply #'max (mapcar #'cadr matches))))
          ;; Semantic Check (Contiguous?)
          (if (< (- end start) (+ (apply #'+ (mapcar (lambda (m) (- (cadr m) (car m))) matches)) 10)) 
              ;; CONTIGUOUS: Paint the whole RGBA block as one unit
              (dragonruby--make-color-overlay start end r g b a 'hash)
            ;; SCATTERED: Paint fragments
            (dolist (m matches)
              (dragonruby--make-color-overlay (nth 0 m) (nth 1 m) r g b a 'hash))))
        ;; Return the limit to advance the main loop
        limit))))

(defun dragonruby--scan-colors ()
  (dragonruby--clear-color-overlays)
  (save-excursion
    
    ;; 1. ARRAYS [r, g, b, (a)?]
    (goto-char (point-min))
    (while (re-search-forward "\\[\\s-*\\([0-9]+\\)\\s-*,\\s-*\\([0-9]+\\)\\s-*,\\s-*\\([0-9]+\\)\\(?:\\s-*,\\s-*\\([0-9]+\\)\\)?\\s-*\\]" nil t)
      (let ((r (string-to-number (match-string 1)))
            (g (string-to-number (match-string 2)))
            (b (string-to-number (match-string 3)))
            (a (when (match-string 4) (string-to-number (match-string 4)))))
        (dragonruby--make-color-overlay (match-beginning 0) (match-end 0) r g b a 'array)))

    ;; 2. HEX 0xRRGGBB
    (goto-char (point-min))
    (while (re-search-forward "0x\\([0-9A-Fa-f]\\{6\\}\\)" nil t)
      (let* ((hex-str (match-string 1))
             (r (string-to-number (substring hex-str 0 2) 16))
             (g (string-to-number (substring hex-str 2 4) 16))
             (b (string-to-number (substring hex-str 4 6) 16)))
        (dragonruby--make-color-overlay (match-beginning 0) (match-end 0) r g b nil 'hex)))

    ;; 4. HASHES {r: ...}
    (goto-char (point-min))
    (while (re-search-forward "\\b\\([rgba]\\):" nil t)
      (let* ((start-pos (match-beginning 0))
             ;; Find limit (brace or arbitrary)
             (brace-end (save-excursion (ignore-errors (up-list 1) (point))))
             (limit (if brace-end (min brace-end (+ (point) 300)) (+ (point) 300))))
        
        ;; Avoid double-processing if overlay exists
        (unless (cl-some (lambda (ov) (and (<= (overlay-start ov) start-pos)
                                           (>= (overlay-end ov) start-pos)))
                         dragonruby--color-overlays)
          
          (let ((new-pos (dragonruby--process-hash-match start-pos limit)))
            (when new-pos
              (goto-char new-pos))))))))

(provide 'dragonruby-color-scanner)
```

### src/colors/dragonruby-color-utils.el
```elisp
;;; dragonruby-color-utils.el --- Color math and conversion utilities -*- lexical-binding: t; -*-

(defun dragonruby--get-contrast-color (r g b)
  "Return \\='white\\=' or \\='black\\=' depending on the brightness of the background color."
  (if (< (+ (* r 0.299) (* g 0.587) (* b 0.114)) 128) "white" "black"))

(defun dragonruby--rgb-to-hex (r g b)
  "Convert RGB values (0-255) to a Hex string #RRGGBB."
  (format "#%02x%02x%02x" (min 255 (max 0 r)) (min 255 (max 0 g)) (min 255 (max 0 b))))

(provide 'dragonruby-color-utils)
```

### src/dragonruby-registry.el
```elisp
;;; dragonruby-registry.el --- Central concept registry -*- lexical-binding: t; -*-

;; The Census of the World.
;; Maps domain concepts (sprites, etc.) to their metadata and handlers.

(defvar dragonruby-registry-table (make-hash-table :test 'eq)
  "Storage for registered concepts.")

(defun dragonruby-registry-register (id props)
  "Register a concept ID with PROPS (plist).
Example:
(dragonruby-registry-register \\='sprite
  \\='(:doc \"docs/concepts/sprite.org\"
    :overlay dragonruby-sprite-overlay))"
  (puthash id props dragonruby-registry-table))

(defun dragonruby-registry-get (id prop)
  "Retrieve PROP for concept ID."
  (plist-get (gethash id dragonruby-registry-table) prop))

(defun dragonruby-registry-all-ids ()
  "Return list of all registered concept IDs."
  (hash-table-keys dragonruby-registry-table))

(provide 'dragonruby-registry)
```

### src/concepts/dragonruby-concept-visuals.el
```elisp
;;; dragonruby-concept-visuals.el --- Visual overlays for concepts -*- lexical-binding: t; -*-

(require 'dragonruby-core)
(require 'dragonruby-events)
(require 'dragonruby-docs)

(defvar dragonruby-concept-keywords
  '("args" "outputs" "inputs" "state" "grid" "geometry" "tick" "sprites" "solids" "borders" "labels")
  "List of DragonRuby concepts to highlight.")

(defvar-local dragonruby--concept-overlays nil
  "List of concept overlays in the current buffer.")

(defun dragonruby--clear-concept-overlays ()
  (mapc #'delete-overlay dragonruby--concept-overlays)
  (setq dragonruby--concept-overlays nil))

(defun dragonruby--activate-concept (concept-name)
  "Action triggered when interacting with a concept."
  (message "🔍 Concept: %s" concept-name)
  ;; Trigger the documentation system
  (let ((doc-file (dragonruby-docs--find-concept-for-symbol concept-name)))
    (if doc-file
        (dragonruby-docs-open doc-file "definition")
      (message "No documentation entry found for '%s'" concept-name))))

(defun dragonruby--make-concept-overlay (start end concept-name)
  (let ((ov (make-overlay start end)))
    ;; Visual Style: Subtle Blue Underline
    (overlay-put ov 'face '(:underline (:color "#61AFEF" :style line)))
    
    ;; Tooltip
    (overlay-put ov 'help-echo (format "Concept: %s (Click or RET to view docs)" concept-name))
    
    ;; Priority (The Shield): Ensure it sits above basic font-lock but doesn't block everything
    (overlay-put ov 'priority 50)
    
    ;; Interaction Map
    (let ((map (make-sparse-keymap)))
      ;; Mouse Interaction
      (define-key map [mouse-1] 
        (lambda () (interactive) (dragonruby--activate-concept concept-name)))
      
      ;; Keyboard Interaction (Enter key when cursor is on the overlay)
      (define-key map (kbd "RET") 
        (lambda () (interactive) (dragonruby--activate-concept concept-name)))
      
      (overlay-put ov 'keymap map))
    
    (push ov dragonruby--concept-overlays)))

(defun dragonruby--scan-concepts ()
  (dragonruby--clear-concept-overlays)
  (save-excursion
    (goto-char (point-min))
    (let ((regexp (concat "\\_<" (regexp-opt dragonruby-concept-keywords) "\\_>")))
      (while (re-search-forward regexp nil t)
        (let ((concept (match-string 0))
              (start (match-beginning 0))
              (end (match-end 0)))
          ;; Check context: Ensure we are not inside a string or comment
          (let ((ppss (syntax-ppss)))
            (unless (or (nth 3 ppss) (nth 4 ppss))
              (dragonruby--make-concept-overlay start end concept))))))))

(defun dragonruby--after-concept-change (_beg _end _len)
  (dragonruby--debounce #'dragonruby--scan-concepts 0.5))

(defun dragonruby--refresh-concepts ()
  (when (and (bound-and-true-p dragonruby-concepts-mode)
             (eq (current-buffer) (window-buffer)))
    (dragonruby--scan-concepts)))

(provide 'dragonruby-concept-visuals)
```

### src/sprites/dragonruby-sprite-actions.el
```elisp
;;; dragonruby-sprite-actions.el --- Interaction handlers for sprites -*- lexical-binding: t; -*-

(require 'dragonruby-events)
(require 'dragonruby-core)
(require 'dragonruby-sprite-fs)

;; Actions subscribe to events. They Decision Makers.

(declare-function w32-shell-execute "w32fns.c")

(defun dragonruby-sprite-actions-open-file (path)
  "Action: Open the source file for PATH or the image itself."
  (let ((abs-path (dragonruby-sprite-fs-resolve-path path)))
    (message "Opening sprite: %s" abs-path)
    ;; Logic for smart jump (source vs image) moved effectively here or in core
    (let ((source (and dragonruby-experimental-smart-jump 
                       (dragonruby--find-source-file abs-path))))
      (if source
          (progn
            (message "Found source file: %s" (file-name-nondirectory source))
            (cond
             ((eq system-type 'windows-nt)
              (w32-shell-execute "open" source))
             ((eq system-type 'darwin)
              (start-process "open" nil "open" source))
             (t
              (start-process "xdg-open" nil "xdg-open" source))))
        ;; Fallback: Open in Emacs
        (find-file abs-path)))))

(defun dragonruby-sprite-actions-init ()
  "Subscribe actions to events."
  (dragonruby-events-on 'sprite:click #'dragonruby-sprite-actions-open-file))

(provide 'dragonruby-sprite-actions)
```

### src/sprites/dragonruby-sprite-overlay.el
```elisp
;;; dragonruby-sprite-overlay.el --- Visual representation for sprites -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'image-file)
(require 'dragonruby-events)
(require 'dragonruby-sprite-fs)
(require 'dragonruby-sprite-model)

(defvar-local dragonruby--sprite-overlays nil
  "List of sprite overlays in the current buffer.")

;; --- pure visuals ---

(defun dragonruby--inline-thumb (abs-path)
  (when (dragonruby-sprite-fs-solid-p abs-path)
    (let ((img (create-image abs-path nil nil :height 20 :ascent 'center)))
      (propertize " " 'display img))))

(defun dragonruby--tooltip (abs-path)
  (if (dragonruby-sprite-fs-solid-p abs-path)
      (let ((img (create-image abs-path nil nil :max-width 300 :max-height 300)))
        (propertize " " 'display img))
    "❌ Invalid/Missing Image"))

(defun dragonruby--trigger-click (path)
  "Trigger the sprite:click event. This decouples UI from Actions."
  (interactive)
  (dragonruby-events-emit 'sprite:click path))

(defun dragonruby--make-overlay (start end raw-path)
  (let* ((abs-path (dragonruby-sprite-fs-resolve-path raw-path))
         (solid (dragonruby-sprite-fs-solid-p abs-path))
         ;; Determine domain state: Valid vs Missing vs Unsupported
         (ov (make-overlay start end)))
    
    ;; "The Shield": Overlays are your exclusive semantic UI
    (overlay-put ov 'face
                 `(:underline (:color ,(if solid "green" "red"))))
    
    (overlay-put ov 'help-echo (dragonruby--tooltip abs-path))
    
    (when solid
      (overlay-put ov 'after-string
                   (concat " " (dragonruby--inline-thumb abs-path)))
      (overlay-put ov 'follow-link t)
      (overlay-put ov 'keymap
                   (let ((map (make-sparse-keymap)))
                     (define-key map [mouse-1]
                       (lambda () (interactive) (dragonruby--trigger-click raw-path)))
                     (define-key map (kbd "RET")
                       (lambda () (interactive) (dragonruby--trigger-click raw-path)))
                     map)))
    
    (push ov dragonruby--sprite-overlays)))

(defun dragonruby--scan-sprites ()
  (mapc #'delete-overlay dragonruby--sprite-overlays)
  (setq dragonruby--sprite-overlays nil)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\"\\([^\"\n]+\\)\"" nil t)
      (let* ((start (match-beginning 1))
             (end (match-end 1))
             (raw-path (match-string 1))
             (ext (downcase (or (file-name-extension raw-path) ""))))
        (when (dragonruby-sprite-model-supported-p ext)
          (dragonruby--make-overlay start end raw-path))))))

(defun dragonruby--after-sprite-change (_beg _end _len)
  ;; Clear specific area logic or full refresh
  (dragonruby--debounce #'dragonruby--scan-sprites 0.3))

(defun dragonruby--refresh-sprites ()
  (when (and (bound-and-true-p dragonruby-sprite-mode)
             (eq (current-buffer) (window-buffer)))
    (dragonruby--scan-sprites)))

(provide 'dragonruby-sprite-overlay)
```

### src/sprites/dragonruby-sprite-completion.el
```elisp
;;; dragonruby-sprite-completion.el --- CAPF for sprites -*- lexical-binding: t; -*-

(require 'dragonruby-sprite-model)
(require 'dragonruby-sprite-fs)

(defun dragonruby--annotate-sprite-candidate (_candidate)
  "Return icon for sprite candidate."
  " 🖼️ Sprite")

(defun dragonruby--make-sprite-completion-table (candidates)
  "Create a completion table with sprite annotations."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (annotation-function . ,#'dragonruby--annotate-sprite-candidate))
      (complete-with-action action candidates string pred))))

(defun dragonruby-sprite-completion-at-point ()
  "Completion backend for Sprites."
  (let ((state (syntax-ppss)))
    (cond
     ;; 1. Inside a String? -> Sprite Completion
     ((nth 3 state)
      (save-excursion
        (goto-char (nth 8 state)) ;; Start of string
        (let* ((string-start (1+ (nth 8 state)))
               (point-now (point))
               ;; Use FS module to get candidates
               (candidates (dragonruby-sprite-fs-find-all)))
          (when (>= point-now string-start)
           (list string-start point-now
                 (dragonruby--make-sprite-completion-table candidates)
                 :exclusive 'no)))))

     ;; 2. At 'spr' symbol? -> Expand to "path/to/sprite.png"
     ((looking-back "\\bspr" (- (point) 3))
      (let* ((start (- (point) 3))
             (end (point))
             (candidates (dragonruby-sprite-fs-find-all)))
        (list start end
              (dragonruby--make-sprite-completion-table candidates)
              :exclusive 'no
              :exit-function
              (lambda (str status)
                (when (eq status 'finished)
                  (save-excursion
                    (goto-char start)
                    (insert "\"")
                    (ignore-errors (forward-char (length str)))
                    (insert "\""))))))) 

     (t nil))))

(provide 'dragonruby-sprite-completion)
```

### src/sprites/dragonruby-sprite-model.el
```elisp
;;; dragonruby-sprite-model.el --- Domain logic for sprites -*- lexical-binding: t; -*-

;; Pure domain definitions. 
;; Doesn't touch disk. Doesn't paint pixels.

(defconst dragonruby-supported-sprites
  '("png" "jpg" "jpeg" "gif" "bmp")
  "Extensions supported by DragonRuby Game Toolkit.")

(defun dragonruby-sprite-model-supported-p (ext)
  "Is EXT a supported sprite extension?"
  (member ext dragonruby-supported-sprites))

(provide 'dragonruby-sprite-model)
```

### src/sprites/dragonruby-sprite-fs.el
```elisp
;;; dragonruby-sprite-fs.el --- Filesystem operations for sprites -*- lexical-binding: t; -*-

(require 'dragonruby-core)
(require 'image-file)

(defun dragonruby-sprite-fs-resolve-path (path)
  "Resolve PATH relative to the project root."
  (let ((root (dragonruby--find-project-root)))
    (expand-file-name path root)))

(defun dragonruby-sprite-fs-solid-p (abs-path)
  "Check if sprite exists and is a valid image file."
  (and (file-exists-p abs-path)
       (condition-case nil
           (image-size (create-image abs-path))
         (error nil))))

(defun dragonruby-sprite-fs-find-all ()
  "Scan disk for all sprite files in the project.
Returns a list of paths relative to project root."
  (let* ((root (dragonruby--find-project-root))
         (files (directory-files-recursively
                 root "\\.\\(png\\|jpg\\|jpeg\\|gif\\|bmp\\)$")))
    (mapcar (lambda (f) (file-relative-name f root)) files)))

(defun dragonruby-sprite-fs-exists-p (path)
  "Check if a sprite exists locally."
  (file-exists-p path))

(provide 'dragonruby-sprite-fs)
```

### src/dragonruby-events.el
```elisp
;;; dragonruby-events.el --- Lightweight internal event bus -*- lexical-binding: t; -*-

;; A semantic layer over Emacs hooks to decouple modules.
;; Modules publish events, other modules subscribe.
;;
;; Usage:
;; (dragonruby-events-on 'sprite:click #'my-handler)
;; (dragonruby-events-emit 'sprite:click sprite-path)

(defvar dragonruby-events-listeners (make-hash-table :test 'eq)
  "Registry of event listeners.
Keys are event symbols, values are lists of functions.")

(defun dragonruby-events-on (event-name callback)
  "Subscribe CALLBACK to EVENT-NAME."
  (let ((current (gethash event-name dragonruby-events-listeners)))
    (puthash event-name (append current (list callback)) dragonruby-events-listeners)))

(defun dragonruby-events-off (event-name callback)
  "Unsubscribe CALLBACK from EVENT-NAME."
  (let ((current (gethash event-name dragonruby-events-listeners)))
    (puthash event-name (remove callback current) dragonruby-events-listeners)))

(defun dragonruby-events-emit (event-name &rest args)
  "Trigger EVENT-NAME with ARGS. Notifies all subscribers synchronously."
  (let ((listeners (gethash event-name dragonruby-events-listeners)))
    (dolist (fn listeners)
      (condition-case err
          (apply fn args)
        (error (message "DragonRuby Event Error [%s]: %S" event-name err))))))

(defun dragonruby-events-clear (event-name)
  "Clear all listeners for EVENT-NAME."
  (remhash event-name dragonruby-events-listeners))

(provide 'dragonruby-events)
```

### src/dragonruby-image-tools.el
```elisp
;;; dragonruby-image-tools.el --- Image viewing and editing tools Facade -*- lexical-binding: t; -*-

;; Facade that exposes the public interface for image tools.

(require 'dragonruby-image-modify "image-tools/dragonruby-image-modify")
(require 'dragonruby-image-view "image-tools/dragonruby-image-view")
(require 'dragonruby-image-ui "image-tools/dragonruby-image-ui")

(defun dragonruby--image-mode-hook ()
  "Activate toolbar if inside a DragonRuby project."
  (when (and buffer-file-name (locate-dominating-file buffer-file-name "app/main.rb"))
    (dragonruby--setup-image-header-line)))

(define-minor-mode dragonruby-image-tools-mode
  "DragonRuby Image Tools."
  :global t
  :group 'dragonruby
  (if dragonruby-image-tools-mode
      (add-hook 'image-mode-hook #'dragonruby--image-mode-hook)
    (remove-hook 'image-mode-hook #'dragonruby--image-mode-hook)))

(provide 'dragonruby-image-tools)
```

### src/dragonruby-sprites.el
```elisp
;;; dragonruby-sprites.el --- Sprite previews and completion Facade -*- lexical-binding: t; -*-

(require 'dragonruby-core)
(require 'dragonruby-events)
(require 'dragonruby-registry)

;; Micro-modules loading
;; Adopting a clean architecture:
;; model: What is a sprite
;; fs:    Where are they
;; overlay: How they look (The LSP Shield)
;; actions: What they do
;; completion: How to write them

(require 'dragonruby-sprite-model  "sprites/dragonruby-sprite-model")
(require 'dragonruby-sprite-fs     "sprites/dragonruby-sprite-fs")
(require 'dragonruby-sprite-actions "sprites/dragonruby-sprite-actions")
(require 'dragonruby-sprite-overlay "sprites/dragonruby-sprite-overlay")
(require 'dragonruby-sprite-completion "sprites/dragonruby-sprite-completion")

(defun dragonruby--enable-capf ()
  (add-hook 'completion-at-point-functions
            #'dragonruby-sprite-completion-at-point nil t))

(define-minor-mode dragonruby-sprite-mode
  "DragonRuby sprite intelligence."
  :lighter " 🖼" ;; Use the icon suggested
  (if dragonruby-sprite-mode
      (progn
        (dragonruby-sprite-actions-init)
        (dragonruby-registry-register 'sprite 
                                     '(:doc "docs/concepts/sprite.org"
                                       :overlay dragonruby-sprite-overlay))
        (dragonruby--enable-capf)
        (add-hook 'after-change-functions #'dragonruby--after-sprite-change nil t)
        (add-hook 'window-configuration-change-hook #'dragonruby--refresh-sprites nil t)
        (dragonruby--scan-sprites))
    (remove-hook 'completion-at-point-functions
                 #'dragonruby-sprite-completion-at-point t)
    (remove-hook 'after-change-functions #'dragonruby--after-sprite-change t)
    (remove-hook 'window-configuration-change-hook #'dragonruby--refresh-sprites t)
    (mapc #'delete-overlay dragonruby--sprite-overlays)
    (setq dragonruby--sprite-overlays nil)))

(provide 'dragonruby-sprites)
```

