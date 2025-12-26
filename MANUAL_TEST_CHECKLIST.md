# ✅ Lista de Verificación Manual - DragonRuby Mode

Sigue estos pasos para verificar que todo funciona correctamente.

## 📋 Preparación

1. **Abrir Emacs**
2. **Cargar el plugin**:
   ```
   M-x load-file RET
   /path/to/EMACS/load-plugin.el RET
   ```
   O simplemente abre un archivo `.rb` con `def tick`

---

## ✅ Test 1: Verificar que el Modo se Activa

1. Abre el archivo: `test-dragonruby-mode.rb`
2. Verifica que en la modeline aparece` **DR**`
3. Si no aparece, ejecuta: `M-x dragonruby-mode`

**Resultado esperado**: 
- ✅ Modeline muestra " DR"

---

## ✅ Test 2: Concepto `tick`

1. En `test-dragonruby-mode.rb`, coloca el cursor sobre la palabra **`tick`** (línea 4)
2. Espera 1 segundo
3. Mira el **minibuffer** (parte inferior de Emacs)

**Resultado esperado**:
```
The heartbeat function called 60 times per second with fresh args. — Think of tick as a movie projector...
```

---

## ✅ Test 3: Concepto `args`

1. Coloca el cursor sobre **`args`** (cualquier línea)
2. Espera 1 segundo

**Resultado esperado**:
```
The central data structure passed to 'tick' every frame (60 times/second)...
```

---

## ✅ Test 4: Concepto `args.state`

1. Coloca el cursor sobre **`args.state`** (línea 11 o 12)
2. Mira el minibuffer

**Resultado esperado**:
```
A dynamic OpenStruct where you store EVERYTHING that must persist between frames.
```

---

## ✅ Test 5: Concepto `args.inputs.keyboard`

1. Coloca el cursor sobre **`args.inputs.keyboard`** (línea 18)
2. Mira el minibuffer

**Resultado esperado**:
```
Read-only hash providing current and historical keyboard state.
```

---

## ✅ Test 6: Concepto `args.outputs.sprites`

1. Coloca el cursor sobre **`args.outputs.sprites`** (línea 34)
2. Mira el minibuffer

**Resultado esperado**:
```
Array queue for textured rectangles (images) with optional transformations.
```

---

## ✅ Test 7: Colores - Preview Visual

1. En `test-dragonruby-mode.rb`, busca las líneas con arrays de colores:
   ```ruby
   red = [255, 0, 0]           # Línea 47
   green = [0, 255, 0, 128]    # Línea 48
   blue = [0, 0, 255, 255, 10] # Línea 49
   ```

**Resultado esperado**:
- ✅ `[255, 0, 0]` tiene un fondo **rojo**
- ✅ `[0, 255, 0, 128]` tiene un fondo **verde**
- ✅ `[0, 0, 255, 255, 10]` tiene un fondo **azul**

---

## ✅ Test 8: Sprites - Hover con Metadata

1. Abre el archivo: `test-sprites.rb`
2. Busca la línea con: `path: "sprites/player.png"` (línea 16)
3. **Pasa el mouse** sobre el texto `"sprites/player.png"`

**Resultado esperado**:
- ✅ Aparece un tooltip con:
  - Miniatura de la imagen (si existe el archivo)
  - Dimensiones (ej: 64x64 px)
  - Formato (PNG)
  - Tamaño del archivo (KB)
  - Ruta completa
  - "💡 Click to open file"

---

## ✅ Test 9: Sprites - Click para Abrir

1. En `test-sprites.rb`, línea 16
2. **Click** sobre `"sprites/player.png"` con el mouse

**Resultado esperado:**:
- ✅ Se abre el archivo de imagen (si existe)
- Si no existe, verás un error (esperado)

---

## ✅ Test 10: Sprites - Visual Feedback

1. En `test-sprites.rb`, observa los paths de sprites
2. Verifica colores:

**Resultado esperado**:
- ✅ Paths válidos: subrayado **cyan**
- ✅ Paths con formato no soportado (gif, svg): subrayado ondulado **naranja**
- ✅ Paths que no existen: subrayado ondulado **rojo**

---

## ✅ Test 11: Inspector de Conceptos

1. Coloca el cursor sobre cualquier concepto (ej: `args.state`)
2. Presiona: **`C-c C-d`**

**Resultado esperado**:
- ✅ Se abre un buffer con información completa del concepto:
  - Name
  - Definition (EN + ES)
  - Intention (EN + ES)
  - Mental Model (EN + ES)
  - Problems
  - Limits
  - Relations

---

## ✅ Test 12: Ajustar Color Interactivo

1. Posiciona el cursor sobre un array de color: `[255, 0, 0]`
2. Presiona: **`C-c C-k`**

**Resultado esperado**:
- ✅ Aparecen prompts para editar:
  - Red (0-255): 255
  - Green (0-255): 0
  - Blue (0-255): 0
- ✅ Al cambiar valores, el color se actualiza automáticamente

---

## 📊 Resumen de Resultados

Marca los tests completados:

- [ ] Test 1: Modo se activa (DR en modeline)
- [ ] Test 2: `tick` muestra definición
- [ ] Test 3: `args` muestra definición
- [ ] Test 4: `args.state` muestra definición
- [ ] Test 5: `args.inputs.keyboard` muestra definición
- [ ] Test 6: `args.outputs.sprites` muestra definición
- [ ] Test 7: Colores tienen preview visual
- [ ] Test 8: Sprites muestran tooltip con metadata
- [ ] Test 9: Click en sprite abre archivo
- [ ] Test 10: Sprites tienen feedback visual correcto
- [ ] Test 11: Inspector funciona (C-c C-d)
- [ ] Test 12: Ajuste de color interactivo (C-c C-k)

---

## 🐛 Si algo no funciona

### El modo no se activa
```
M-x dragonruby-mode
```

### No se ven las definiciones
1. Verifica que eldoc está activo: `M-x eldoc-mode`
2. Recarga el plugin: `M-x load-file RET load-plugin.el RET`

### Los colores no se ven
1. Verifica configuración:
   ```
   M-x customize-variable RET dragonruby-enable-color-preview RET
   ```
2. Asegúrate que esté en 't' (true)

### Los sprites no se ven
1. Verifica configuración:
   ```
   M-x customize-variable RET dragonruby-enable-sprite-preview RET
   ```
2. Asegúrate que esté en 't' (true)

---

## ✅ Si TODOS los tests pasan

¡Felicidades! El sistema está funcionando al 100% 🎉

Todos los conceptos están registrados y funcionando:
- ✅ tick
- ✅ args, args.state, args.inputs, args.outputs
- ✅ args.inputs.keyboard, args.inputs.mouse
- ✅ args.outputs.sprites, args.outputs.labels, args.outputs.solids
- ✅ color-array

---

**Fecha**: 2025-12-24  
**Versión**: 0.1.1
