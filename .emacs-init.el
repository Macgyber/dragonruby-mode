;; Configuración de inicio para DragonRuby Mode
;; Este archivo se carga automáticamente al iniciar Emacs

;; Desactivar pantalla de bienvenida
(setq inhibit-startup-screen t)

;; Activar tema oscuro (wombat)
(load-theme 'wombat t)

;; Cargar DragonRuby Mode
(add-to-list 'load-path "e:/ANTIGRAVITY/dragonruby-mode/src")
(add-to-list 'load-path "e:/ANTIGRAVITY/dragonruby-mode/src/core")
(add-to-list 'load-path "e:/ANTIGRAVITY/dragonruby-mode/src/ui")
(add-to-list 'load-path "e:/ANTIGRAVITY/dragonruby-mode/src/mode")
(add-to-list 'load-path "e:/ANTIGRAVITY/dragonruby-mode/src/concepts")

(require 'dragonruby)

;; ACTIVAR DRAGONRUBY-MODE AUTOMÁTICAMENTE
;; Método 1: Hook en ruby-mode
(add-hook 'ruby-mode-hook #'dragonruby-mode)
;; Método 2: Hook en ruby-ts-mode (Emacs 29+)
(add-hook 'ruby-ts-mode-hook #'dragonruby-mode)
;; Método 3: Activar después de abrir cualquier archivo .rb
(add-hook 'find-file-hook
          (lambda ()
            (when (and (buffer-file-name)
                       (string-match-p "\\.rb$" (buffer-file-name)))
              (dragonruby-mode 1))))

;; Mensaje de bienvenida
(message "DragonRuby Mode cargado y se activa automáticamente en .rb")
