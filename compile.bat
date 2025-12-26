@echo off
REM Script para compilar dragonruby-mode a bytecode
echo ========================================
echo Compilando DragonRuby Mode...
echo ========================================
echo.

"C:\Program Files\Emacs\emacs-29.4\bin\emacs.exe" --batch -l compile-dragonruby.el

echo.
echo ========================================
echo Compilacion completada!
echo ========================================
echo.
echo Los archivos .elc han sido generados.
echo Ahora dragonruby-mode cargara mas rapido.
echo.
pause
