# 🐛 Debug: Colores no se Muestran

## Problema
Los overlays de colores NO aparecen en el buffer.

## Diagnóstico Paso a Paso

### Paso 1: Verificar que el modo está activo

1. Abre `test-dragonruby-mode.rb`
2. Verifica que en la modeline dice: **" DR"**
3. Si NO dice " DR":
   ```
   M-x dragonruby-mode
   ```

---

### Paso 2: Verificar configuración

Ejecuta en Emacs:

```
M-: dragonruby-enable-color-preview RET
```

**Debe mostrar**: `t`

Si muestra `nil`, actívalo:
```
M-x customize-variable RET dragonruby-enable-color-preview RET
```
Cambia a `t` (true) y guarda.

---

### Paso 3: Test Manual Rápido

1. En Emacs, ejecuta:
   ```
   M-x load-file RET quick-color-test.el RET
   ```

2. Debería abrir un buffer con:
   ```ruby
   # Test colors
   red = [255, 0, 0]
   ```

3. El array `[255, 0, 0]` **DEBE tener fondo rojo**

**Si NO tiene fondo rojo**: Hay un problema con los overlays en tu Emacs

---

### Paso 4: Forzar Re-scan

Si el modo está activo pero NO ves colores:

1. Abre `test-dragonruby-mode.rb`
2. Ejecuta:
   ```
   M-: (dragonruby--scan-all) RET
   ```

3. Deberías ver los colores aparecer

---

### Paso 5: Verificar que los overlays existen

Ejecuta:
```
M-: (length (overlays-in (point-min) (point-max))) RET
```

**Debe mostrar** un número > 0 (ej: `5`, `10`, etc.)

Si muestra `0`: NO se están creando overlays

---

### Paso 6: Debug Manual Completo

Ejecuta:
```
M-x load-file RET test-colors-debug.el RET
M-x test-color-overlay RET
```

Verás un buffer con información de debug mostrando:
- Configuración actual
- Cuántos overlays se crearon
- Si hay algún error

---

## Posibles Causas

### Causa 1: Configuración desactivada
**Solución**: Ver Paso 2

### Causa 2: Modo no activado
**Solución**: Ver Paso 1

### Causa 3: Regex no encuentra colores
**Solución**: Verifica que el formato sea exactamente:
```ruby
[255, 0, 0]         # ✅ Funciona
[255,0,0]           # ✅ Funciona
[ 255 , 0 , 0 ]     # ✅ Funciona
{r: 255, g: 0}      # ❌ NO funciona (no es array)
```

### Causa 4: El scan no se ejecutó
**Solución**: Ver Paso 4 (forzar re-scan)

### Causa 5: Terminal Emacs sin soporte GUI
Si estás usando Emacs en terminal (no GUI), los overlays de colores pueden no funcionar correctamente.

**Verifica**: `M-: (display-graphic-p) RET`
- Si muestra `t`: OK, estás en modo gráfico
- Si muestra `nil`: Estás en terminal, los colores pueden no verse

---

## Test Rápido de Concepto

Ejecuta este código en Emacs para crear UN overlay manualmente:

```emacs-lisp
M-: (progn
      (goto-char (point-min))
      (when (re-search-forward "\\[255, 0, 0\\]" nil t)
        (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
          (overlay-put ov 'face '(:background "#ff0000" :foreground "white"))
          (message "Overlay created!"))))
RET
```

Si esto NO muestra fondo rojo, el problema es con Emacs, no con nuestro código.

---

## Solución Temporal

Si nada funciona, recarguemos TODO desde cero:

```
M-x dragonruby-mode RET    ; Desactiva
M-x load-file RET load-plugin.el RET    ; Recarga plugin
M-x dragonruby-mode RET    ; Reactiva
```

Luego:
```
M-: (dragonruby--scan-all) RET
```

---

## ¿Qué Deberías Ver?

En un archivo con:
```ruby
red = [255, 0, 0]
green = [0, 255, 0]
blue = [0, 0, 255]
```

Deberías ver:
- `[255, 0, 0]` con fondo **ROJO**
- `[0, 255, 0]` con fondo **VERDE**
- `[0, 0, 255]` con fondo **AZUL**

---

**Si después de todos estos pasos NO funciona**, por favor comparte:
1. Versión de Emacs: `M-x emacs-version`
2. Modo gráfico o terminal: `M-: (display-graphic-p)`
3. Resultado de: `M-: dragonruby-enable-color-preview`
