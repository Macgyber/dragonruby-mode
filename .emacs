;;; .emacs --- DragonRuby Local Dev Configuration (PURE LOGIC)

;; ============================================================
;; ⚠️ NO CAMBIOS VISUALES - SOLO LÓGICA DEL PLUGIN
;; ============================================================

;; Define la raíz del proyecto dinámicamente
(defvar dragonruby-project-root 
  (file-name-directory (or load-file-name buffer-file-name))
  "Root of the dragonruby-mode project.")

;; ============================================================
;; 1. Rutas de carga para el Plugin
;; ============================================================
(dolist (path '("" "src" "src/core" "src/sprites" "src/colors" 
                "src/paths" "src/image-tools" "src/concepts"))
  (add-to-list 'load-path (expand-file-name path dragonruby-project-root)))

;; ============================================================
;; 2. Cargar el Plugin (con manejo de errores)
;; ============================================================
(condition-case err
    (require 'dragonruby-mode)
  (error (message "❌ Error cargando dragonruby-mode: %s" err)))

;; Activar features opcionales por defecto para testing
(setq dragonruby-enable-sprites t)
(setq dragonruby-enable-colors t)
(setq dragonruby-enable-paths t)
(setq dragonruby-enable-image-tools t)

;; Auto-activar en archivos Ruby (futuros)
(add-hook 'ruby-mode-hook #'dragonruby-maybe-enable)
(add-hook 'ruby-ts-mode-hook #'dragonruby-maybe-enable)

;; ============================================================
;; 3. AUTO-ENABLE EN BUFFERS YA ABIERTOS
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
;; 4. Teclas de Recarga (Dev Tools) - SIEMPRE DISPONIBLES
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
  ;; Desactivar modo en buffer actual
  (when (bound-and-true-p dragonruby-mode)
    (ignore-errors (dragonruby-mode -1)))
  
  ;; Recargar todos los archivos .el
  (let ((src-dir (expand-file-name "src" dragonruby-project-root))
        (errors nil))
    (dolist (file (directory-files-recursively src-dir "\\.el$"))
      (condition-case err
          (load-file file)
        (error 
         (push (format "%s: %s" (file-name-nondirectory file) err) errors))))
    
    ;; Recargar el archivo principal
    (condition-case err
        (load-file (expand-file-name "dragonruby-mode.el" dragonruby-project-root))
      (error (push (format "dragonruby-mode.el: %s" err) errors)))
    
    ;; Mostrar errores si los hay
    (if errors
        (message "⚠️ Recargado con errores:\n%s" (string-join errors "\n"))
      (progn
        ;; Activar en todos los buffers Ruby
        (dragonruby--activate-in-all-ruby-buffers)
        (message "🔄 DragonRuby Mode: Recargado!")))))

;; SIEMPRE registrar las teclas, incluso si hay errores arriba
(global-set-key (kbd "<f5>") 'reload-emacs-config)
(global-set-key (kbd "<f6>") 'reload-dragonruby-mode)

(message "🚀 DragonRuby Mode Cargado")
