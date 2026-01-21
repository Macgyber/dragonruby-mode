# 🌌 Stargate: Temporal State Management (Experimental)

Stargate provides a robust framework for recording and replaying game states, enabling precise debugging and deterministic state management. It is built on a **Modular Architecture** where every module is responsible for a single specialized task.

---

## 🕹️ Operational Controls (Management)

Use these commands to manage the Stargate runtime and session state:

### ⚡ `M-x dragonruby-stargate--initialize-runtime`
**Refresh Runtime Components.**
Re-aggregates all Ruby implementation modules into `mygame/stargate_portal.rb` and triggers a hot-reload in DragonRuby.
- **Use when**: You encounter syntax errors in the DragonRuby console or after updating the plugin.

### 📊 `M-x stargate-status` (Alias: `stargate-status`)
**The System Dashboard.**
Opens the visual status interface for monitoring the state of the simulation.
- **Keybindings**: 
  - `F7` / `r` : **Resume/Record** (Enable state capture)
  - `F8` / `p` : **Pause** (Disable state capture)
  - `g` : Refresh the Status Dashboard
  - `q` : Close the Dashboard

### 🛑 `M-x dragonruby-stargate-emergency-halt`
**Emergency Process Termination.**
Forcefully terminates the `dragonruby.exe` process at the operating system level.
- **Use when**: The engine becomes unresponsive or trapped in an infinite loop.

---

## 🏛️ System Components (Modular Architecture)

To maximize stability and maintainability, Stargate is divided into independent specialized modules:

| Component | Emacs Module | Ruby Module | Responsibility |
| :--- | :--- | :--- | :--- |
| **Command Bus** | `recorder.el` | `commands.rb` | Handling user control signals (Resume/Pause). |
| **Telemetry** | `telemetry.el` | `protocol.rb` | Processing and filtering state data from the engine. |
| **Persistence** | `sessions.el` | `state.rb` | Managing the storage and retrieval of session history. |
| **RAM Cache** | (N/A) | `black_box.rb` | High-speed circular buffer for zero-lag state navigation. |
| **Orchestrator** | `fsm.el` | `clock.rb` | Managing state transitions and simulation synchronization. |
| **Bridge** | `bridge.el` | (Sentinel) | Managing the IPC pipe and process lifecycle monitoring. |
| **Navigator** | `timeline.el` | `timeline.rb` | Logic for branching and temporal state transitions. |

---

## 🚦 Troubleshooting

1. **MOMENT_COUNT is 0**: Ensure recording is enabled (Press `F7`). Verify that the `.stargate/` directory is writable.
2. **Syntax Error in Console**: Execute `M-x dragonruby-stargate--initialize-runtime` to refresh the runtime components.
3. **Emacs Unresponsive on Engine Exit**: The **Process Monitor** (Sentinel) should prevent this. Ensure `dragonruby-stargate-bridge.el` is correctly loaded.
4. **Input Lag**: The **BlackBox** cache now handles history in RAM. If lag persists, check for excessive log output in the DragonRuby console.

---

*Stargate v1.1.2 — Experimental Phase*
