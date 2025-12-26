@echo off
REM Script para instalar DragonRuby Mode permanentemente en Emacs
echo ========================================
echo Instalando DragonRuby Mode en Emacs
echo ========================================
echo.

REM Copiar configuracion a archivo de usuario de Emacs
copy /Y ".emacs-init.el" "%USERPROFILE%\.emacs"

echo.
echo ========================================
echo Instalacion completada!
echo ========================================
echo.
echo DragonRuby Mode se cargara automaticamente
echo la proxima vez que abras Emacs.
echo.
echo Para abrir Emacs ahora:
echo   emacs
echo.
echo O para abrir con un archivo:
echo   emacs examples\01_hello_world.rb
echo.
pause
