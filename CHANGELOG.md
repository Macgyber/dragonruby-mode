# Changelog

Todos los cambios notables del proyecto están documentados en este archivo.

El formato está basado en [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) y este proyecto sigue [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

---

---

<details open>
<summary><h2>[0.5.0] - 2026-01-05 (FASE 3: AUDIO, FONTS & POLISH)</h2></summary>

### Añadido
- **Motor de Resonancia (Audio)**:
  - **Validación Emerald**: Assets de audio válidos (`.ogg`, `.wav`) se resaltan en verde vibrante (`#2ECC71`)
  - **Centinela de Formatos**: Detección instantánea de formatos no soportados (`.mp3`) marcados en rojo
  - **Tooltips de Metadata**: Información de peso de archivo al pasar el ratón
  - **Iconos Contextuales**: 🔊 para música (`.ogg`), 📢 para efectos (`.wav`)
- **Motor de Glifos (Fuentes)**:
  - **Aura Cian**: Rutas de fuentes válidas (`.ttf`, `.otf`) brillan en cian (`#00FFFF`)
  - **Previsualizaciones Adaptativas al Tema**: Miniaturas que detectan automáticamente tu tema de Emacs (oscuro/claro) y ajustan colores de fondo/texto para máxima visibilidad
  - **Cache Inteligente**: Las previsualizaciones incluyen el modo del tema en el hash, regenerándose automáticamente al cambiar entre temas
  - **Detección de Formatos No Soportados**: archivos `.woff`, `.woff2`, `.eot` marcados en naranja con tooltip educativo
  - **Visor Interactivo**: Sistema completo con Pangrams, ABCs y texto de muestra

### Mejorado
- **Navegación Contextual CAPF**:
  - **Filtrado Inteligente**: El sistema detecta el contexto (`require`, `read_file`, `path:`) y muestra solo archivos relevantes
  - **Experiencia Visual Mejorada**: Comparación clara entre trabajar con y sin el plugin en el minibuffer
  - **Sin Fricción**: Snippet `spr` + doble `C-M-i` muestra únicamente sprites válidos, eliminando ruido
- **Refinamiento de Colores**:
  - **Detección Técnica Precisa**: Se han eliminado referencias incorrectas a símbolos (`:red`, `:indigo`) que DragonRuby no soporta nativamente
  - **Documentación Honesta**: Solo se documentan formatos realmente soportados: Hex (`0xFF00FF`), Arrays RGB/RGBA, y Hashes `{r:_, g:_, b:_}`
- **Resolución de Rutas Inteligente**:
  - **Fallback Robusto**: Si no se encuentra la raíz del proyecto, el sistema busca archivos relativos al directorio actual
  - **Eliminación de Falsos Positivos**: Las rutas solo se marcan en rojo si estamos 100% seguros de que el archivo falta
  - **Estado Neutral**: Si el contexto es incierto, el sistema no muestra errores (filosofía: "si no estás seguro, no asustes al usuario")

### Removido
- **Smart Source Finder**: Eliminada la característica de búsqueda automática de archivos `.psd`/`.ase` para sprites
  - Razón: Simplificar la lógica y seguir una filosofía "DragonRuby-First" sin suposiciones externas
- **Experimental Smart Jump**: Removida la flag `dragonruby-experimental-smart-jump`
  - Razón: Funcionalidad ya no presente en el core
- **Cursor Auto-Jump Promise**: Eliminada la promesa de "salto automático del cursor fuera de comillas"
  - Razón: Honestidad técnica - la característica no funciona de forma consistente en la práctica

### Cambiado
- **Tema-Awareness Completo**: Fonts, Audio y Sprites ahora adaptan sus colores según el tema activo del usuario
- **Colores Semánticos Unificados**:
  - Verde (`#2ECC71`) = Válido (Audio)
  - Cian (`#00FFFF`) = Válido (Fonts)
  - Azul (`#2196F3`) = Válido (Paths/Code)
  - Naranja = Formato no soportado
  - Rojo = Archivo faltante o error

### Corregido
- **Generación de Font Previews**: Ahora usan un directorio temporal del sistema como fallback si no se encuentra `.dr_history`
- **Syntax Error en Path Completion**: Corregido paréntesis extra que impedía la carga del módulo
- **Click Interaction Removal**: Eliminados todos los bindings de `mouse-1` en overlays para mantener filosofía keyboard-first

### Filosofía
Esta versión marca un punto de inflexión en la **Honestidad Técnica**:
- ✅ Solo documentamos lo que funciona al 100%
- ✅ Eliminamos promesas que no podemos cumplir
- ✅ Cada feature está probada en producción
- ✅ El README refleja la realidad del código, no aspiraciones

</details>

---

<details>
<summary><h2>[0.4.0] - 2026-01-04 (FASE 2: VISUALS & ASSETS)</h2></summary>

### Añadido
- **Sistema de Fuentes Inteligente**: Nueva suite dedicada a la tipología en DragonRuby.
  - **Live Previews**: Al escribir un path (TTF/OTF) en el código, se muestra una previsualización dinámica.
  - **Visualizador Profesional**: Nuevo Major Mode (`DR-Font`) para abrir archivos de fuente directamente.
  - **Interfaz de Usuario**: Botones en el header-line para ver "Muestra completa", "Set de Caracteres" o "Pangramas".
  - **Motor Independiente**: Lógica hospedada exclusivamente en `src/fonts/`, sin dependencias cruzadas con el editor de imágenes.
- **DaFont en Creative Hub**: Añadido enlace directo a DaFont para la descarga de recursos tipográficos.

### Cambiado
- **Filosofía 100% Keyboard-First**: Se ha eliminado el soporte de ratón (`mouse-1`) de los overlays semánticos después de pruebas ergonómicas.
  - **Estandarización**: Se utiliza exclusivamente `C-c C-o` para interactuar con sprites, paths y conceptos.
  - **Ergonomía**: La tecla `RET` (Enter) se mantiene libre para escritura fluida, evitando aperturas accidentales.
  - **Ventanas Redimensionables**: El panel de documentación ahora permite el redimensionamiento nativo (ratón en bordes o atajos `C-x { }`), eliminando la rigidez de versiones anteriores.
  - **Herramientas de Precisión**: El ratón queda reservado únicamente para su propósito natural en herramientas como Piskel o el Selector de Color (cuando está habilitado).
  - **Tooltips**: Actualizados para reflejar el nuevo atajo `C-c C-o`.
  - **Excepción**: Los selectores de color (`■`) usan `mouse-1` (herramienta de precisión) solo si el picker está habilitado.
- **Documentación en Side Window**: Sistema de documentación ahora abre archivos `.org` en panel lateral derecho
  - Ventana persistente (35% ancho, lado derecho)
  - **Texto Fluido**: Habilitado `visual-line-mode` por defecto; el texto se ajusta automáticamente al ancho de la ventana, eliminando el scroll horizontal y los recortes.
  - Permite navegación simultánea: código a la izquierda, docs a la derecha
  - Workflow tipo IDE moderno
- **Presets solo para software libre**: Removidas referencias a software comercial de presets de editores
  - **Removidos**: Aseprite, Photoshop, Pixelmator Pro, Affinity Photo, Paint.NET
  - **Mantenidos**: GIMP, Krita, Inkscape (herramientas libres/gratuitas)
  - Los usuarios pueden añadir cualquier editor (comercial o libre) vía botón `[+]` del Creative Hub
  - Ejemplos en documentación ahora usan solo software libre

### Removido
- **Photopea del Creative Hub**: Eliminado del conjunto de herramientas web predefinidas
  - Razón: Alineación con filosofía de software libre
  - Los usuarios pueden re-añadirlo manualmente si lo desean vía `[+]`
- **Referencias a Aseprite**: Eliminadas todas las menciones específicas a Aseprite del código y documentación
  - Extensión `.aseprite` removida de `dragonruby-sprite-source-extensions`
  - Añadida `.kra` (Krita) en su lugar
  - Ejemplos en docs ahora usan `.psd`, `.kra`, `.xcf` (formatos genéricos)
  - Razón: No hacer publicidad de software comercial, mantener neutralidad

### Documentado
- **SHORTCUTS.md**: Actualizado para reflejar solo keybindings de teclado
  - Removidas referencias obsoletas a `mouse-1`, `S-mouse-1`, `C-c p`
  - Confirmado que snippet `spr` funciona correctamente
- **CONTRIBUTING.md**: Corregidos enlaces rotos a `ARCHITECTURE.md` y `CONTRACT.md`
- **IMAGE_EDITOR.md, INSTALLATION.md, README.md**: Todos actualizados con ejemplos de software libre

### Notas de Migración
Si actualizas desde v0.3.0:
- Los clicks simples y la tecla `RET` ya NO activan los overlays (sprites, paths, conceptos).
- Usa exclusivamente `C-c C-o` cuando el cursor esté sobre el elemento para interactuar.
- Esto libera la tecla `Enter` (`RET`) para su función original de crear nuevas líneas sin interrupciones.
- Photopea ya no aparece en Creative Hub por defecto (puedes re-añadirlo con `[+]`)
- Si configuraste `dragonruby-external-image-editor` con Aseprite, sigue funcionando, pero los presets ahora sugieren GIMP/Krita

</details>

---

<details>
<summary><h2>[0.3.0] - 2026-01-03</h2></summary>

### Añadido
- **Creative Hub**: Nuevo panel de herramientas creativas integrado en el header-line
  - Arquitectura de botón padre: El botón `CREATIVE` funciona como otros grupos de herramientas (VIEW, TRANSFORM, etc.), expandiéndose para revelar botones hijos
  - Accesos directos a herramientas web: Graphite, Photopea, Piskel, Lospec e Itch.io directamente desde el header-line
  - Gestión de herramientas personalizadas:
    - Botón **[+] Add Tool**: Añade herramientas personalizadas con nombre, URL/ruta y color (color vibrante aleatorio sugerido)
    - Botón **[-] Hide/Remove**: Oculta herramientas predefinidas o elimina completamente las creadas por el usuario
  - Colores personalizables: Cada herramienta puede tener su propio color, con colores vibrantes aleatorios generados por defecto
  - Configuración persistente: Herramientas guardadas mediante el sistema customize de Emacs, persisten entre sesiones
- **Sistema de advertencias de seguridad**: Sistema de advertencias interactivas estandarizado
  - Guard "In Development": Intentar usar funciones experimentales o deshabilitadas ahora muestra un aviso profesional en español con botón "Cerrar"
  - Tolerancia a fallos mejorada: Errores de activación en `dragonruby-mode` ahora se capturan y reportan mediante el sistema de advertencias en lugar de fallar silenciosamente
  - Guard interactivo de color: Hacer clic en swatches de color cuando el picker está deshabilitado informa al usuario sobre el estado de desarrollo de la función
- **Sistema de assets centralizado**: Centralización de infraestructura
  - Movidas definiciones de extensiones de archivo y conocimiento de assets a `src/core/dragonruby-assets.el`
  - Módulos con cero dependencias: Refactorizados módulos `paths` y `sprites` para eliminar dependencias cruzadas
  - Comunicación exclusiva a través de la infraestructura Core
  - Compatibilidad hacia atrás: Mantenidos aliases legacy para asegurar estabilidad interna
- **Aislamiento absoluto de módulos**: Completitud de refactor core
  - Movidos Events y Registry a `src/core/`
  - El root del proyecto es ahora la capa base para todos los demás
  - Eliminación de dependencias: Aislamiento absoluto entre módulos funcionales
  - Cada parte puede ser removida sin afectar la estabilidad del modo

### Mejorado
- **Autocompletado consciente del contexto**: El sistema de paths ahora reconoce el contexto del código
  - Filtrado inteligente: Sistema de autocompletado reconoce contexto del código (`.sprites`, `.labels`, `.require`, etc.)
  - Modo solo-sprites: Filtra automáticamente archivos Ruby y Data cuando se detecta un path o contexto tipo sprite, reduciendo drásticamente el ruido
  - UI simplificada: Listas de autocompletado más cortas y 100% relevantes al contexto actual de escritura
- **Herramientas de imagen (Estética y lógica renovadas)**:
  - UI fluida y adaptativa: Implementación de header-line "líquido". Botones y etiquetas ahora se reducen dinámicamente (`VIEW` → `V` → 👁️) y espaciados colapsan basándose en el ancho de ventana para prevenir desbordamiento de UI
  - Lógica de acordeón: Abrir un grupo de herramientas colapsa automáticamente los demás, asegurando un espacio de trabajo limpio y enfocado sin importar el tamaño de ventana
  - Sistema de navegación de timeline: Reemplazado el botón "Undo" único con botones versionados `Back (<)` y `Forward (>)`
  - Historial no-destructivo: Cada edición guarda un snapshot en directorio oculto `.dr_history`, permitiendo navegación profunda del historial de edición
  - "Debug Stage" (Visual Ray-X): El botón `Info` ahora alterna un fondo gris oscuro de alto contraste (#333333) para revelar márgenes transparentes, facilitando recortes de precisión
  - Resaltado de estado activo: Los encabezados de grupo ahora se "iluminan" (fondo coloreado del tema) cuando están expandidos, proporcionando feedback visual inmediato del contexto activo
  - Reorganización de workflow: Botones reordenados por prioridad. Navegación (`<`, `>`) movida al grupo `VIEW`; `Info` y herramientas de sistema unificadas bajo `SYSTEM`

### Corregido
- **Estabilidad de buffer-revert**: Corregido bug donde los grupos "explotaban" (auto-expandirse) después de cada modificación de imagen. Grupos ahora por defecto en estado colapsado
- **Fix de refresh en macOS**: Forzada recarga instantánea de buffer para navegación de Timeline para asegurar actualizaciones visuales en tiempo real
- **Preservación de pixel-art**: Actualizados comandos de resize para usar `-filter point` para escalado con bordes nítidos
- **Protección de minibuffer**: Añadidos guards para prevenir errores "minibuffer while in minibuffer"

### Limpieza de código
- **Popup removido**: El buffer popup de Creative Hub fue removido en favor de interacción exclusiva via header-line
- **Modeline más limpio**: Cambiado indicador de modo de `DR-Img` a `🎨` para apariencia más limpia

### Documentación
- **Actualizado IMAGE_EDITOR.md**: Añadida documentación completa del Creative Hub
- **Transparencia y comunidad**:
  - Developer Mode Disclosure: Añadida sección "Micro-switches" a la documentación para testing de acceso temprano
  - Plantillas de GitHub Issue: Formularios estandarizados para Feature Requests y Feedback Experimental
  - Documentación para desarrolladores: Renovado `CONTRIBUTING.md` para invitar colaboración en funciones "In Development"
- **Política de binarios limpios**: Todos los archivos `.elc` ahora se eliminan después de auditorías/pruebas de compilación para asegurar un estado limpio para usuarios

</details>

---

<details>
<summary><h2>[0.2.0] - 2026-01-02</h2></summary>

### Añadido
- **Arquitectura en tiempo real y rendimiento**:
  - Debounce multi-canal: Timers independientes para Paths, Colors y Sprites. Previene colisiones de módulos y asegura reactividad instantánea mientras se escribe
  - Cache de proyecto por buffer: Optimizada detección de project root para eliminar I/O redundante a disco durante escaneos
  - Escaneo atómico: Implementados `save-match-data` y `save-restriction` en todos los escaneos periódicos para prevenir interferencia con operaciones del usuario
  - Lógica micro-modular: Finalizado el aislamiento de módulos core para mejor recuperación de errores

### Mejorado
- **Paths (Estabilizado)**:
  - Enlaces de hipertexto: Paths ahora son links azules en negrita. Validados instantáneamente (50ms - 100ms) después de escribir
  - CAPF universal: Autocompletados listan todos los archivos del proyecto sin filtrado restrictivo
  - Renovación de snippets: Añadidos snippets `spr` (sprite) y `script` (load_script). Corregido posicionamiento de cursor dentro de comillas
- **Colores**:
  - Swatches visuales: Swatches escalan con tamaño de fuente y proporcionan feedback sobre transparencia
  - Feature flag de picker: Añadido `dragonruby-enable-picker` (default `nil`). Deshabilitando el picker interactivo mientras se perfecciona para evitar confusión
- **Herramientas de imagen**:
  - Chequeo unificado de ImageMagick: Comandos ahora detectan automáticamente si ImageMagick falta y muestran un menú interactivo con link directo de descarga
  - UI mejorada: Aplicado estilo premium de botones al header-line de imagen
  - Sistema de Undo: Integrados backups automáticos antes de cualquier operación destructiva de ImageMagick

### Corregido
- **Colisiones de timer**: Corregido bug donde el escaneo de color cancelaba la detección de paths
- **Afinidad de buffer**: Funciones debounced ahora ejecutan estrictamente en el buffer correcto
- **Código obsoleto**: Purgados todos los archivos `.elc` para asegurar que solo el código fuente más reciente esté activo
- **Integridad de sintaxis**: Resueltos errores `end-of-file` en `dragonruby-utils.el`

### Documentación
- **Refactor de README**: Actualizado con los nuevos detalles de Hipertexto y Arquitectura
- **Actualización de contrato**: Formalizada la regla "Fault-Tolerant Multi-channel"

</details>

---

<details>
<summary><h2>[0.1.0] - 2025-12-29</h2></summary>

### Añadido
- **Refactor arquitectónico mayor**:
  - Core modular: División de archivos monolíticos en sub-módulos enfocados (`src/core/`, `src/paths/`, `src/colors/`, `src/image-tools/`)
  - Patrón Facade: Módulos principales ahora actúan como facades orquestando lógica especializada
  - Estructura de grado empresarial: Limpiadas dependencias y enforzado "One Module, One Responsibility"
- **Sistema avanzado de colores**:
  - Soporte Alpha: Detecta y visualiza transparencia en arrays (`[r,g,b,a]`) y hashes (`{r:_, a:_}`)
  - Color Picker: Añadido botón interactivo "Edit Color" (`■`) junto a valores de color
  - Accesibilidad: Swatches de color escalan con tamaño de fuente y soportan edición local
- **Visuales de conceptos**:
  - Nuevo módulo `src/concepts` escaneando keywords de DragonRuby (`args`, `state`, `tick`)
  - Subrayados interactivos sutiles y no-invasivos conectando a documentación
- **Herramientas de imagen**:
  - Modularización completa de herramientas de modificación y visualización de imágenes

### Corregido
- **Byte-Compile**: Validado que todo el código esté limpio de errores de byte-compile
- **Docstrings**: Formato estandarizado a través de todos los módulos

</details>

---

<details>
<summary><h2>[0.0.2] - 2025-12-28</h2></summary>

### Añadido
- **Autocomplete**: Mejorado autocomplete de `require` (`req + C-M-i`) para sugerir inteligentemente archivos `.rb`. Añadido soporte para autocompletado de archivos de datos (json, txt, csv) en contextos de string
- **Sprites mejorados**: Autocomplete de sprites (`spr + C-M-i`) ahora inserta paths de string apropiados y muestra iconos `🖼️`
- **Robustez**: Implementado sistema de "Feature Flag" para habilitar/deshabilitar módulos específicos individualmente
- **Interactividad**: Corregidos overlays de sprites para ser completamente clicables (soporta Mouse-1, Mouse-2, Enter) usando propiedad `follow-link`
- **Configuración**: Expuestos `dragonruby-unsupported-sprites` y `dragonruby-data-extensions` como opciones de usuario customizables (`defcustom`)

### Experimental (Deshabilitado por defecto)
- **Living Documentation**: Sistema para vincular símbolos de código a conceptos `.org` locales
- **Smart Source Jumping**: Habilidad de abrir archivos `.aseprite` desde el visor de imágenes

</details>

---

<details>
<summary><h2>[0.0.1] - 2025-12-24</h2></summary>

### Añadido
- Lanzamiento inicial de **dragonruby-mode**
- Resaltado semántico para colores (arrays RGB, hashes, hex)
- Previews de sprites (thumbnails inline y tooltips hover)
- Navegación básica de paths (file paths clicables)
- Detección automática de proyecto (`app/main.rb`)

</details>

