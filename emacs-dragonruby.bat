@echo off
REM Script robusto para abrir Emacs con DragonRuby Mode
REM Compila automaticamente y abre los ejemplos de preview

echo ========================================
echo [1/2] Compilando DragonRuby Mode...
echo ========================================
"C:\Program Files\Emacs\emacs-29.4\bin\emacs.exe" --batch -l "e:\ANTIGRAVITY\dragonruby-mode\compile-dragonruby.el"

echo.
echo ========================================
echo [2/2] Abriendo Emacs con Ejemplos...
echo ========================================

REM Si no se pasan argumentos, abrir los 2 ejemplos de preview por defecto
if "%~1"=="" (
    "C:\Program Files\Emacs\emacs-29.4\bin\emacs.exe" -l "e:\ANTIGRAVITY\dragonruby-mode\.emacs-init.el" "e:\ANTIGRAVITY\dragonruby-mode\examples\04_color_preview.rb" "e:\ANTIGRAVITY\dragonruby-mode\examples\05_sprite_preview.rb"
) else (
    "C:\Program Files\Emacs\emacs-29.4\bin\emacs.exe" -l "e:\ANTIGRAVITY\dragonruby-mode\.emacs-init.el" %*
)

