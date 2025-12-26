# 🚀 Guía Rápida de Pruebas - DragonRuby Mode

## 📁 Archivos de Prueba Creados

| Archivo | Propósito |
|---------|-----------|
| `MANUAL_TEST_CHECKLIST.md` | Lista de verificación paso a paso (12 tests) |
| `DEBUG_COLORS.md` | **🔴 EMPIEZA AQUÍ** - Debug para colores que no se ven |
| `test-dragonruby-mode.rb` | Archivo Ruby con ejemplos de conceptos |
| `test-sprites.rb` | Archivo Ruby con ejemplos de sprites |
| `quick-color-test.el` | Test ultra-rápido de overlays |
| `test-colors-debug.el` | Debug completo del sistema de colores |

---

## 🐛 PROBLEMA: Colores no se muestran

### Solución Rápida

1. **Abre**: `DEBUG_COLORS.md`
2. **Sigue los pasos** del 1 al 6
3. **Identifica** qué paso falla

### Test Más Rápido (30 segundos)

En Emacs:

```
M-x load-file RET quick-color-test.el RET
```

**Resultado esperado**: Ver un buffer con `[255, 0, 0]` en fondo ROJO

- ✅ **Si lo ves rojo**: Los overlays funcionan, el problema es otro
- ❌ **Si NO es rojo**: Problema con overlays de Emacs

---

## ✅ Si los Overlays Funcionan

Entonces el problema es que el scan no se está ejecutando.

### Forzar Scan Manual

1. Abre `test-dragonruby-mode.rb`
2. Asegúrate que `dragonruby-mode` está activo (ve " DR" en modeline)
3. Ejecuta:
   ```
   M-: (dragonruby--scan-all) RET
   ```

**Debería**: Crear todos los overlays de colores inmediatamente

---

## 📊 Verificar Estado del Sistema

### Comando de Diagnóstico

```emacs-lisp
M-: (progn
      (message "=== DragonRuby Mode Status ===")
      (message "Mode active: %s" dragonruby-mode)
      (message "Color preview enabled: %s" dragonruby-enable-color-preview)
      (message "Max overlays: %s" dragonruby-max-overlays-per-type)
      (message "Overlays in buffer: %d" (length (overlays-in (point-min) (point-max))))
      (message "Color overlays: %d" 
               (cl-count-if (lambda (ov) (overlay-get ov 'dragonruby-color-overlay))
                            (overlays-in (point-min) (point-max)))))
RET
```

**Deberías ver** en el minibuffer:
```
Mode active: t
Color preview enabled: t
Max overlays: 50
Overlays in buffer: 15
Color overlays: 5
```

---

## 🔧 Recarga Completa del Plugin

Si nada funciona, recarga TODO:

```
; 1. Desactiva el modo
M-x dragonruby-mode RET

; 2. Recarga el plugin
M-x load-file RET load-plugin.el RET

; 3. Reactiva el modo
M-x dragonruby-mode RET

;4. Fuerza un scan
M-: (dragonruby--scan-all) RET
```

---

## 🎯 Lista de Verificación Mínima

Antes de reportar un bug, verifica:

- [ ] Emacs está en modo gráfico: `M-: (display-graphic-p)` → debe ser `t`
- [ ] Modo activado: Hay " DR" en la modeline
- [ ] Config activada: `M-: dragonruby-enable-color-preview` → debe ser `t`
- [ ] Test rápido pasa: `quick-color-test.el` muestra color rojo
- [ ] Scan ejecutado: `M-: (dragonruby--scan-all)` crea overlays

---

## 📞 Información para Reportar

Si después de todo NO funciona, necesito:

1. **Versión de Emacs**:
   ```
   M-x emacs-version
   ```

2. **Modo gráfico o terminal**:
   ```
   M-: (display-graphic-p)
   ```

3. **Estado de configuración**:
   ```
   M-: dragonruby-enable-color-preview
   M-: dragonruby-mode
   ```

4. **Resultado del test rápido**:
   ¿`quick-color-test.el` muestra rojo?

5. **Overlays creados**:
   ```
   M-: (length (overlays-in (point-min) (point-max)))
   ```

---

## 🎉 Si Todo Funciona

Continúa con: `MANUAL_TEST_CHECKLIST.md` para probar las 12 características completas.

---

**Creado**: 2025-12-24  
**Para debug de**: Color overlays not showing
