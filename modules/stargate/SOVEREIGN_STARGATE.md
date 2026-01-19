# 🌌 Sovereign Stargate: The Machine of Time

Stargate is a revolutionary state management and synchronization module for DragonRuby. It allows you to **record, rewind, and branch** your game's timeline directly from Emacs.

## 🚀 Getting Started

1.  **Launch your game**: Use `M-x dragonruby-run`.
2.  **Activate Stargate**: Press `<F7>` or `C-c C-s r`. You will see a red circle `🔴` in the modeline indicating the "Synchronization Link" is active.
3.  **Play**: Focus on your game and play normally. Stargate is recording every state behind the scenes.

## ⌨️ Keybindings (The Architect's Console)

| Key | Action | Description |
|:---|:---|:---|
| `<F7>` | **Sync / Record** | Start recording frames and syncing state. |
| `C-c C-s r` | **Sync / Record** | Same as <F7>. |
| `C-c C-s p` | **Pause** | Enter "Stasis Mode". Useful for debugging a specific frame. |
| `C-c C-s j` | **Jump** | Open the **Branch Forest** timeline to jump to any point in history. |
| `C-c C-s t` | **Timeline** | Toggle the visual timeline buffer. |
| `C-c C-s l` | **Load Session** | Restore a previously saved time-travel session. |

## 🧬 Core Concepts

### 1. The Branch Forest (Timeline)
Stargate doesn't just record a linear video; it records a **forest of possibilities**.
- **Moments**: Each frame is a "Moment" with a checksum.
- **Branches**: You can "Fork" the timeline at any point to try different code injections without losing your previous history.
- **Divergence (⚡)**: If you change the game state manually (via console), Emacs will detect a "Divergence" and show a lightning bolt icon. You can jump back to a stable moment to fix it.

### 2. The Guardian (Law XII)
If you modify your Ruby code in an external editor, Stargate will detect the mismatch and enter **STASIS**. The simulation will pause automatically to prevent you from recording "dirty" states that don't match your code in Emacs.

### 3. State Integrity
Every moment is hashed using SHA-256. This ensures that when you "Jump" to the past, you are exactly where you were, down to the last bit of the RNG seed.

### 4. The Portal Strategy
To bypass DragonRuby's strict sandbox, Stargate uses a "Portal" approach. Emacs creates a temporary `mygame/stargate_portal.rb` which is loaded by the engine. This portal securely hooks the internal `tick` loop and redirects load paths to the Stargate runtime without needing manual project modifications.

### 5. The Void Shield (Zero-Lag IPC)
DragonRuby can generate thousands of log lines per second (especially during window resizing). Stargate implements a **Void Shield** in the Emacs Bridge that discards engine noise at high speed before it touches the UI thread, ensuring Emacs remains responsive even under heavy log pressure.

---
*Created by Antigravity for the DragonRuby Architect.*
*v0.8.1 - Industrial Stability Edition*
