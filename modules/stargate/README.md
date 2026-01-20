# 🌌 Stargate: Live Simulation Debugging for DragonRuby

**Status**: v0.8.1 (File-Based Communication Model)  
**Philosophy**: Observer, not controller

---

## What is Stargate?

Stargate is a **time-travel debugging system** for DragonRuby games. It records every frame of your game as it runs, allowing you to:

- **Rewind** to any previous moment
- **Inspect** state at specific frames
- **Branch** timelines when you change code mid-game
- **Replay** deterministically from any point

**Key Insight**: Your game never stops running. Stargate observes, it doesn't control.

---

## How It Works

### The Model

```
You write code → DragonRuby hot-reloads → Game continues running
                                              ↓
                                    Events printed to STDOUT
                                              ↓
                                    Emacs reads and indexes
                                              ↓
                                    Timeline visualization
```

**Stargate doesn't send commands to DragonRuby.** It writes files and listens to events.

---

## Quick Start

### 1. One-Time Setup

Add this snippet to the **top** of your `mygame/main.rb`:

```ruby
# Stargate Auto-Loader
portal = 'mygame/stargate_portal.rb'
require portal if File.exist?(portal)
```

**Why?** This allows DragonRuby's hot-reload to load Stargate when you activate it.

### 2. Activate Stargate

1. Open your DragonRuby project in Emacs
2. Launch your game: `M-x dragonruby-run`
3. Press **F7** (Stargate Infect)
4. If prompted, allow Emacs to install the loader automatically
5. Wait 1-2 seconds for hot-reload to detect the portal
6. Look for modeline indicator: `🌌` (Stargate Active)

### 3. Use Timeline

- **F8**: Open Stargate Timeline buffer
- **F9**: Record/pause timeline
- **Click** on any frame in timeline to jump to that moment

---

## Key Bindings

| Key | Command | Description |
|-----|---------|-------------|
| **F7** | `dragonruby-stargate-infect` | Activate Stargate in running game |
| **F8** | `dragonruby-stargate-timeline` | Open timeline visualization |
| **F9** | `dragonruby-stargate-record` | Start/resume recording |
| **F10** | `dragonruby-stargate-pause` | Pause recording |

---

## How Stargate Respects DragonRuby

### What Stargate Does ✅

- Writes `.rb` files to disk
- Reads `STDOUT` for game events
- Indexes moments in `session.json`
- Never blocks the 60 FPS loop

### What Stargate Doesn't Do ❌

- Send commands via STDIN (DragonRuby isn't a REPL)
- Pause or freeze the game
- Control execution remotely
- Break determinism

**Philosophy**: *"Emacs is the chronicler, not the commander."*

---

## Understanding the Modeline Indicator

| Icon | Meaning |
|------|---------|
| 💤 | Searching for DragonRuby process |
| ⏳ | Waiting for hot-reload to detect portal |
| 🌌 | Stargate active and syncing |
| ⚡ | Divergence detected (state mismatch) |

---

## Troubleshooting

### F7 does nothing / stays at ⏳

**Cause**: `main.rb` doesn't have the auto-loader snippet

**Fix**:
1. Check if this line is at the top of `mygame/main.rb`:
   ```ruby
   portal = 'mygame/stargate_portal.rb'
   require portal if File.exist?(portal)
   ```
2. If missing, add it manually or let F7 install it automatically
3. Save `main.rb` to trigger hot-reload

### STARGATE::INFECTED never appears

**Cause**: DragonRuby's hot-reload hasn't detected the portal file yet

**Wait**: Up to 2 seconds for file watcher to trigger

**Check**: Look at DragonRuby's console output for reload messages

### Timeline doesn't show frames

**Cause**: Recording isn't active

**Fix**: Press **F9** to start recording

---

## File-Based Communication (Technical Note)

Stargate v0.8.1 migrated from a broken STDIN-based model to DragonRuby's native hot-reload mechanism.

**Before (Broken)**:
- Emacs sent commands via STDIN
- DragonRuby ignored them (not a REPL)
- System appeared to "hang"

**Now (Correct)**:
- Emacs writes files to `mygame/`
- DragonRuby's file watcher detects changes
- Hot-reload loads code automatically
- No blocking, no latency, no broken states

**See**: `legends/HOT_RELOAD_CONTRACT.md` (Law XIX) for full technical details

---

## Session Files

Stargate saves your timeline to `sessions/session.json`.

**Features**:
- Portable across machines
- Survives Emacs restarts
- Contains full game state history
- Deterministic replay via seed tracking

**Share** session files with teammates to debug their exact playthrough.

---

## Design Philosophy

Stargate follows the **Tomorrow Corporation debugging model**:

> "The game is always running. The debugger doesn't pause execution; it just gives you a different view of the same living simulation."

**Alignment**:
- ✅ 60 FPS never blocked
- ✅ State preserved during hot-reload
- ✅ Deterministic replay via RNG seeding
- ✅ Timeline as **view**, not **controller**

---

## Comparison with Traditional Debuggers

| Feature | Traditional Debugger | Stargate |
|---------|---------------------|----------|
| **Execution** | Pauses/steps | Continuous (60 FPS) |
| **Time travel** | No | Yes (rewind to any frame) |
| **State inspection** | At breakpoints only | Every frame recorded |
| **Code changes** | Restart required | Hot-reload preserves state |
| **Determinism** | Not guaranteed | Enforced via seed tracking |

---

## Advanced Features

### Branching Timelines

When you jump back in time and change code, Stargate creates a new timeline branch:

```
Prime Timeline (Original)
  ↓
  Frame 100 ← You jump here
  ↓
  Change code
  ↓
Branch A (New Reality) ← New timeline from frame 100
```

**Both timelines coexist** in the session file.

### Deterministic Replay

Stargate ensures every replay produces identical results:

- RNG seeds captured per frame
- External inputs recorded
- State hashes validate correctness
- Divergence detection warns of desync

---

## Future Roadmap

### Phase 7 (Planned)
- File-based `pause`/`resume` commands
- Temporary file injection for debugging snippets
- Timeline branching UI
- Visual state diff viewer

### Phase 8+ (Exploration)
- Ruby-level profiler integration
- Execution timestamps per frame
- Object watch system (feasible subset)

**See**: `legends/TOMORROW_CORP_GAP_ANALYSIS.md` for detailed feature analysis

---

## FAQ

### Q: Do I need to keep the auto-loader in production?

**A**: It's safe to keep. The `if File.exist?` check is zero-cost if the portal file isn't present.

### Q: Does Stargate affect performance?

**A**: Minimal impact. State hashing and JSON serialization add ~1ms per frame in typical games.

### Q: Can I use Stargate with existing DragonRuby games?

**A**: Yes! Just add the one-line loader to `main.rb`. No other changes needed.

### Q: What if I don't use Emacs?

**A**: Stargate is Emacs-only currently. The file-based model could be adapted to other editors, but would require porting the timeline UI and session management.

---

## Technical Documentation

- **Law XIX**: `legends/HOT_RELOAD_CONTRACT.md` (Communication model)
- **Laws I-XVIII**: `legends/stargate.md` (Philosophical foundation)
- **Design Canon**: `legends/TOMORROW_CORP_GAP_ANALYSIS.md` (Feature boundaries)
- **Implementation**: `legends/IMPLEMENTATION_COMPLETE_2026-01-20.md` (Migration details)

---

## License

Same as dragonruby-mode (see main README)

---

## Credits

**Philosophy**: Inspired by Tomorrow Corporation's debugging tools and Jonathan Blow's design principles

**Implementation**: Built on DragonRuby's native hot-reload mechanism

**Motto**: *"Cuando el motor es soberano, el debugger deja de romper el tiempo."*

---

🌌 **Stargate is now aligned with reality.** 🐉
