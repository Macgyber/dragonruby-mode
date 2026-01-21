# 🌌 Stargate: Temporal State Management for DragonRuby

**Status**: v1.0.0 Stable (Modern Communication Model)  
**Philosophy**: Observer, not controller

---

## What is Stargate?

Stargate is a **time-travel debugging system** for DragonRuby games. It records every frame of your game as it runs, allowing you to:

- **Rewind** to any previous moment
- **Inspect** state at specific frames
- **Branch** timelines when you change code mid-game
- **Replay** deterministically from any point

**Control Logic**: Your game never stops running. Stargate observes and synchronizes, it doesn't freeze or block execution.

---

## How it Works

### Communication Model

```
You write code → DragonRuby hot-reloads → Game continues running
                                              ↓
                                    Events printed to STDOUT
                                              ↓
                                    Emacs reads and indexes
                                              ↓
                                    Timeline visualization
```

**Stargate uses file-based communication (Law XIX).** It writes files to trigger actions in DragonRuby and listens to the STDOUT stream for telemetry.

---

## Quick Start

### 1. One-Time Setup

Add this snippet to the **top** of your `mygame/main.rb`:

```ruby
# Stargate Auto-Loader
portal = 'mygame/stargate_portal.rb'
require portal if File.exist?(portal)
```

**Why?** This allows DragonRuby's hot-reload to load the Stargate bridge when you activate it.

### 2. Activate Stargate

1. Open your DragonRuby project in Emacs.
2. Launch your game: `M-x dragonruby-run`.
3. Press **F7** to initialize.
4. If prompted, allow Emacs to install the loader automatically.
5. Wait for the modeline indicator: `🌌` (Stargate Active).

### 3. Use Timeline

- **F8**: Open the Timeline buffer.
- **F9**: Start/Stop recording.
- **Click/RET** on any frame in the timeline to jump to that state.

---

## Key Bindings

| Key | Command | Description |
|-----|---------|-------------|
| **F7** | `dragonruby-stargate-record` | Activate and Start Synchronization |
| **F8** | `dragonruby-stargate-pause` | Pause Synchronization |
| **F9** | `dragonruby-stargate-timeline` | Open Timeline Visualization |

---

## Technical Standards

### What Stargate Does ✅

- Writes `.rb` files to disk for commands.
- Reads `STDOUT` for game events and telemetry.
- Indexes moments in `session.json` for persistence.
- Maintains 60 FPS without interruption.

### What Stargate Doesn't Do ❌

- Send commands via STDIN (Not supported by the engine).
- Pause or freeze the engine execution.
- Mutate the past (Historical states are immutable).
- Break game determinism.

**Motto**: *"Emacs is the chronicler, the runtime is the reality."*

---

## Status Indicators (Modeline)

| Icon | Meaning |
|------|---------|
| 💤 | Searching for DragonRuby process |
| ⏳ | Initializing (Waiting for hot-reload) |
| 🌌 | Active and Synchronized |
| ⚡ | Divergence detected (Manual jump required) |

---

## Technical Documentation

- **Law XIX**: `legends/HOT_RELOAD_CONTRACT.md` (Communication model)
- **Architecture**: `knowledge/STARGATE.md` (High-level vision)
- **Status Dashboard**: `M-x dragonruby-stargate-status-buffer`

---

🌌 **Stargate v1.0.0 Stable** 🐉
