;;; .emacs --- DragonRuby Local Dev Configuration (PURE LOGIC)

;; ============================================================
;; ⚠️ ARCHITECTURE v0.7.0 (Lego System)
;; ============================================================

;; Define la raíz del proyecto dinámicamente
(defvar dragonruby-project-root 
  (file-name-directory (or load-file-name buffer-file-name))
  "Root of the dragonruby-mode project.")

;; ============================================================
;; 1. Cargar el Bootloader (Plugin Entry Point)
;; ============================================================
;; El Bootloader (dragonruby-mode.el) se encarga de:
;; - Configurar load-path para modules/
;; - Cargar el Kernel
;; - Registrar los manifestos
(add-to-list 'load-path dragonruby-project-root)

(condition-case err
    (require 'dragonruby-mode)
  (error (message "❌ Error cargando dragonruby-mode: %s" err)))

;; -----------------------------------------------------------------------------
;; ⚙️ Perfil de Usuario (Legos)
;; -----------------------------------------------------------------------------
;; El núcleo de dragonruby-mode (v0.7.1) arranca ultra-ligero por defecto.
;; Forzamos el apagado de todo excepto lo que tú desees activar:

(setq dragonruby-enable-completion t)      ; ✅ ON (Motor de Autocompletado)

;; Descomenta la que necesites (esto sobrescribe el default del núcleo):
(setq dragonruby-enable-colors nil)        ; 🔴 OFF
(setq dragonruby-enable-sprites nil)       ; 🔴 OFF
(setq dragonruby-enable-sprite-tools nil)  ; 🔴 OFF
(setq dragonruby-enable-fonts nil)         ; 🔴 OFF
(setq dragonruby-enable-font-tools nil)    ; 🔴 OFF
(setq dragonruby-enable-audio nil)         ; 🔴 OFF
(setq dragonruby-enable-paths nil)         ; 🔴 OFF
(setq dragonruby-enable-concepts t)        ; ✅ ON (Focused Learning)
(setq dragonruby-enable-docs t)            ; ✅ ON (Org Documentation)

(setq dragonruby-concepts-debug nil)       ; Debug Atómico DESACTIVADO

;; ============================================================
;; 2. AUTO-ENABLE EN BUFFERS YA ABIERTOS
;; ============================================================
(defun dragonruby--activate-in-all-ruby-buffers ()
  "Activate dragonruby-mode in all currently open Ruby buffers."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (or (derived-mode-p 'ruby-mode)
                     (derived-mode-p 'ruby-ts-mode)
                     (string-match-p "\\.rb$" (or (buffer-file-name) "")))
                 (not (bound-and-true-p dragonruby-mode)))
        (condition-case err
            (dragonruby-mode 1)
          (error (message "❌ Error en buffer %s: %s" (buffer-name) err)))))))

;; Ejecutar AHORA para buffers ya abiertos
(dragonruby--activate-in-all-ruby-buffers)

;; ============================================================
;; 3. Teclas de Recarga (Dev Tools)
;; ============================================================

;; F5 -> Recargar ESTE archivo .emacs
(defun reload-emacs-config ()
  "Recargar configuración local."
  (interactive)
  (condition-case err
      (progn
        (load-file (expand-file-name ".emacs" dragonruby-project-root))
        (message "✅ Lógica local recargada!"))
    (error (message "❌ Error recargando: %s" err))))

;; F6 -> Recargar DragonRuby Mode (Hot Reload)
(defun reload-dragonruby-mode ()
  "Recargar código fuente del plugin y reiniciar."
  (interactive)
  (when (bound-and-true-p dragonruby-mode)
    (ignore-errors (dragonruby-mode -1)))
  
  ;; Recargar Entry Point (esto dispara la recarga de módulos y Kernel)
  (load-file (expand-file-name "dragonruby-mode.el" dragonruby-project-root))
  
  (dragonruby--activate-in-all-ruby-buffers)
  (message "🔄 DragonRuby Mode: Recargado (Lego System)!"))

(global-set-key (kbd "<f5>") 'reload-emacs-config)
(global-set-key (kbd "<f6>") 'reload-dragonruby-mode)
(global-set-key (kbd "<f12>") 'find-file) ;; Quick open

;; ============================================================
;; 4. Dependencias del Sistema (ImageMagick)
;; ============================================================
;; Requerido para ver "mini-fuentes" y previsualizaciones
(let ((magick-path "C:/Program Files/ImageMagick-7.1.1-Q16-HDRI/"))
  (when (file-directory-p magick-path)
    (add-to-list 'exec-path magick-path)
    (setenv "PATH" (concat magick-path ";" (getenv "PATH")))
    (message "🖼️ ImageMagick Configurado: %s" magick-path)))

(message "🚀 DragonRuby Mode Cargado")

;;; .emacs ends here
