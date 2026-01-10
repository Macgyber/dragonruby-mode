# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

---

<details open>
<summary><h2>[0.7.1] - 2026-01-10 (SINCRO TOTAL & SMART DOT)</h2></summary>

### 📜 Contrato de Sincronía Total
- **Ultra-Minimalist Core**: Nuevo estándar de fábrica donde **todos** los módulos opcionales (`colors`, `sprites`, `fonts`, `audio`, etc.) están desactivados por defecto, **excepto completion**. El plugin ahora arranca como una herramienta puramente de productividad.
- **Paridad Código-Doc**: Sincronización absoluta entre los `defcustom` del código y las tablas de "Pieza Lego" en el README. Sin letras pequeñas.
- **Smart Dot (Punto Inteligente)**: El autocompletado se dispara automáticamente al escribir un punto, **solo** si sigue a una cadena válida del contrato (ej. `args.`). Mejora radical del flujo de escritura.

### 🧠 Inteligencia Nativa
- **Detección Fallback**: El sistema ahora detecta automáticamente el `dragonruby_api.yml` global en la carpeta del plugin si no existe uno local.
- **Redundancia de Atajos**: Añadido soporte explícito para `C-M-i` junto al atajo amigable `C-.`.
- **Auto-Inserción de Punto**: Al completar una raíz (como `arg` -> `args`), el sistema inserta el punto automáticamente para continuar la cadena.

### 🐛 Bugs Corregidos
- **Modos Obsoletos**: Corregidos checks a modos menores que ya no existen en arquitectura Lego:
  - `dragonruby-font-overlay.el`: `dragonruby-font-mode` → `dragonruby-mode`
  - `dragonruby-audio-overlay.el`: `dragonruby-audio-mode` → `dragonruby-mode`
  - `dragonruby-sprite-overlay.el`: `dragonruby-sprite-mode` → `dragonruby-mode`
  - `dragonruby-concept-visuals.el`: `dragonruby-concepts-mode` → `dragonruby-mode`
- **Path Overlay**: Corregido typo `dragonruby-data-extensions` → `dragonruby-data-exts`.
- **dragonruby-utils.el**: Corregida función `dragonruby--get-image-type` con paréntesis faltante.

</details>

<details>
<summary><h2>[0.7.0] - 2026-01-09 (FASE 5: LEGO ARCHITECTURE & KERNEL)</h2></summary>

### 🏗️ Arquitectura Lego (The Kernel)
El sistema ha sido reestructurado desde cero. Ya no es una colección de scripts, es un **Sistema Operativo** modular.
- **The Kernel**: Un orquestador central que gestiona la vida y muerte de cada funcionalidad.
- **The Three Laws**:
  1. **Namespace Law**: Cada módulo posee su espacio exclusivo.
  2. **Capability Law**: Los módulos declaran qué *necesitan* (`:rendering`) y qué *proveen*, no a quién conocen.
  3. **Cold Boot Law**: Nada corre por defecto. Cero zombies.

### 🛡️ Modularidad Total
Todos los sistemas han sido encapsulados en `modules/` con contratos estrictos (`manifest`):
- `modules/core`: Kernel y librerías base.
- `modules/sprites`: Motor de renderizado.
- `modules/sprites/tools`: Editor de imágenes (depende de sprites).
- `modules/fonts`: Visor de tipografías.
- `modules/audio`, `modules/colors`, `modules/paths`, `modules/concepts`.

### 📦 Preparación MELPA
- Estructura compatible con empaquetado estándar.
- `dragonruby-pkg.el` añadido.
- Configuración flexible: El usuario puede desactivar piezas (`legos`) individuales en su `init.el`.

</details>

<details>
<summary><h2>[0.6.1] - 2026-01-08 (FASE 4: CONTRACT COMPLETION & FLUIDITY)</h2></summary>

### 🧠 Inteligencia Nativa (Autocomplete Engine)
El sistema de autocompletado ha sido estabilizado y verificado.
- **Auto-Dot Flow**: Al seleccionar una raíz como `args`, el sistema inserta automáticamente el punto (`args.`), permitiendo una escritura fluida (`args.` -> `state`).
- **Native Data**: Implementado como backend CAPF estándar. Compatible 100% con `Minibuffer`, `Company-Mode` y `Corfu`.
- **Zero-Ghost Policy**: Código auditado y purgado de referencias a módulos obsoletos. Logs de depuración eliminados para rendimiento máximo.
- **Namespace Safety**: backend renombrado para garantizar cero colisiones con otras utilidades.
- **Contract Fallback**: Busca `dragonruby_api.yml` en la raíz del proyecto; si no existe, usa un contrato global de respaldo.

### 🏭 Mejoras Técnicas
- **Windows Shortcut**: Implementado `C-.` como atajo nativo para disparar autocompletado en DragonRuby Mode.
- **Silent Core**: Eliminada etiqueta visual `[Contract]` para una integración UI más limpia y nativa.

</details>

<details>
<summary><h2>[0.6.0] - 2026-01-06 (FASE INDUSTRIAL: ZERO BLOCKING & RELIABILITY)</h2></summary>

### 🏭 Blindaje Industrial ("Por Fuera vs Por Dentro")
Esta versión representa una reingeniería completa bajo la filosofía de **"Orden Invisible"**.

### Añadido
- **Métricas Activas en Carga**:
  - El sistema de activación (`dragonruby-mode.el`) ahora reporta errores críticos con precisión quirúrgica (`CRITICAL FALLBACK`), permitiendo diagnósticos inmediatos.
  - Eliminación de fallos silenciosos en la carga de módulos.

### Mejorado
- **Zero Blocking (Rendimiento Extremo)**:
  - **Refactor de Sprites**: Eliminada *toda* generación de imágenes del hilo principal de escaneo.
  - **Resultado**: El escaneo de archivos grandes es ahora instantáneo (~0ms bloqueo). Las previsualizaciones ricas (imágenes completas) se cargan *lazy* solo al hacer hover (200ms), manteniendo el editor "liviano como la seda".
- **Memoria Controlada (SRE)**:
  - **Singleton Timer Pattern**: Implementado control estricto de timers en los popups de sprites.
  - **Prevención de Fugas**: Se garantiza que solo exista un timer activo a la vez, eliminando el riesgo de "Timer Storms" al mover el mouse rápidamente.
  - **Ciclos Claros**: Desactivar el modo ahora limpia agresivamente todos los recursos visuales y procesos pendientes.
- **Núcleo Silencioso (Silent Core)**:
  - **Defensa en Profundidad**: `dragonruby-project.el` ahora maneja contextos nulos (buffers sin archivo) sin lanzar excepciones, garantizando estabilidad total en scratchpads y terminales.

### Cambiado
- **Separación de Responsabilidades Visuales**:
  - **Inline**: Solo muestra mini-thumbnails cacheados (Validación instantánea).
  - **Popup**: Maneja exclusivamente la carga de medios ricos (Detalle bajo demanda).
  - Esta separación es la clave de la nueva arquitectura "Zero Blocking".

</details>

---

<details>
<summary><h2>[0.5.0] - 2026-01-05 (FASE 3: AUDIO, FONTS & POLISH)</h2></summary>

### Añadido
- **DragonRuby Creative Hub (v1)**:
  - Integración completa con editores externos.
  - Botones web para Graphite, Piskel, Lospec, Itch.io.
  - Sistema "Adaptive UI" que cambia según el ancho de ventana.
  - Usuarios pueden añadir sus propias "Creative Tools" (URLs o Exes).
- **Sistema de Audio (Experimental)**:
  - Detección de `args.audio`.
  - Reproducción `.wav`/`.ogg` básica desde Emacs (depende de backend).
- **Fuentes (Fonts)**:
  - Previsualización de `.ttf` al hacer hover sobre cadenas.
  - Instalación de fuentes (placeholder).

### Mejorado
- **Image Editor**:
  - Añadido botón "Rotate 90°".
  - Añadido botón "Flip H/V".
  - Mejorado el layout responsivo (VIEW, TRANSFORM, COLOR, SYSTEM, CREATIVE).

</details>

<details>
<summary><h2>[0.4.0] - 2026-01-04 (FASE 2: PATHS & REFACTOR)</h2></summary>

### Añadido
- **Sistema de Paths (Navegación)**:
  - Detección inteligente de `require`, `read_file`, `write_file`.
  - Detección de cadenas JSON y CSV.
  - Enlaces clickeables (Open file).
- **Refactorización Modular**:
  - Separación estricta: `core`, `sprites`, `paths`, `colors`.
  - Eliminación de dependencias circulares.
  - Carga diferida (autoloads).

</details>

<details>
<summary><h2>[0.3.0] - 2026-01-03 (FASE 1: SPRITES & COLORS)</h2></summary>

### Añadido
- **Sistema de Sprites**:
  - Miniaturas inline (tamaño fuente).
  - Tooltip básico.
- **Sistema de Colores**:
  - Detección de Arrays RGB `[255, 0, 0]`.
  - Detección de Hashes `{r: 255, ...}`.
  - Overlay de color real.

</details>

<details>
<summary><h2>[0.1.0] - 2026-01-01 (INICIO)</h2></summary>

### Añadido
- Estructura base del proyecto.
- `dragonruby-mode.el` (esqueleto).
- Detección básica de archivos `.rb`.

</details>
