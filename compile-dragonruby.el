;;; compile-dragonruby.el --- Script para compilar dragonruby-mode -*- lexical-binding: t; -*-

;; Este script compila todos los archivos .el de dragonruby-mode a bytecode (.elc)
;; para mejorar el rendimiento de carga.

(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

;; Agregar directorios al load-path para resolver dependencias
(let ((default-directory (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "src"))
  (add-to-list 'load-path (expand-file-name "src/core"))
  (add-to-list 'load-path (expand-file-name "src/ui"))
  (add-to-list 'load-path (expand-file-name "src/mode"))
  (add-to-list 'load-path (expand-file-name "src/concepts"))
  
  (message "========================================")
  (message "Compilando DragonRuby Mode...")
  (message "========================================")
  
  ;; Compilar en orden de dependencias
  (let ((files-to-compile
         '("src/core/dragonruby-core-concepts.el"
           "src/core/dragonruby-registry.el"
           "src/ui/dragonruby-eldoc.el"
           "src/ui/dragonruby-inspector.el"
           "src/ui/dragonruby-concept-hints.el"
           "src/concepts/dragonruby-tick.el"
           "src/concepts/dragonruby-args.el"
           "src/concepts/dragonruby-args-sub.el"
           "src/concepts/dragonruby-outputs-sub.el"
           "src/concepts/dragonruby-inputs-sub.el"
           "src/mode/dragonruby-impl.el"
           "src/dragonruby.el")))
    
    (dolist (file files-to-compile)
      (let ((full-path (expand-file-name file)))
        (when (file-exists-p full-path)
          (message "Compilando %s..." file)
          (byte-compile-file full-path)))))
  
  (message "========================================")
  (message "✓ Compilación completada!")
  (message "========================================"))

;;; compile-dragonruby.el ends here
