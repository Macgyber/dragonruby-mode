# Stargate Testing Protocol

**Version**: 0.8.1 (File-Based Model)  
**Date**: January 20, 2026  
**Purpose**: Surgical validation of core functionality

---

## Philosophy

> "No pruebes todo a la vez. Hazlo como cirujano, no como gamer."

Each test validates **one specific property** of the system. Tests are ordered by dependency: core → natural → edge cases.

---

## Test 1: Minimal Infection (F7)

**Purpose**: Validate the core infection ritual

### Setup
- Fresh DragonRuby project (empty or minimal `main.rb`)
- `main.rb` does NOT have the Stargate loader yet
- DragonRuby running (`M-x dragonruby-run`)

### Steps
1. Press **F7** in Emacs
2. Observe prompt: `"Stargate requires a one-line loader in main.rb. Install automatically?"`
3. Choose **"yes"**
4. Wait 1-2 seconds

### Expected Results
✅ **Loader added to `main.rb`**:
```ruby
# ═══════════════════════════════════════════════════════════════
# Stargate Auto-Loader (Law XIX: Hot Reload Contract)
# ───────────────────────────────────────────────────────────────
portal = 'mygame/stargate_portal.rb'
require portal if File.exist?(portal)
# ═══════════════════════════════════════════════════════════════
```

✅ **`main.rb` saved automatically**

✅ **DragonRuby console shows reload message**:
```
[INFO] Reloading changed Ruby files.
```

✅ **Portal file created**: `mygame/stargate_portal.rb`

✅ **DragonRuby console shows**:
```
STARGATE::INFECTED
```

✅ **Emacs modeline changes**: `💤` → `⏳` → `🌌`

### Failure Modes

❌ **Modeline stays at ⏳ for >5 seconds**:
- Check if `main.rb` was actually saved
- Check DragonRuby console for syntax errors
- Verify portal file exists

❌ **Prompt doesn't appear**:
- Check if loader is already present (means detection worked!)
- Should skip prompt and go straight to infection

❌ **`STARGATE::INFECTED` never appears**:
- Check DragonRuby console for load errors
- Verify `$LOAD_PATH` includes runtime directory
- Check bootstrap.rb for syntax errors

### Pass Criteria
- Modeline shows `🌌` within 3 seconds
- No manual intervention required after "yes" prompt
- Portal file exists and is valid Ruby

---

## Test 2: Hot-Reload Natural Behavior

**Purpose**: Validate that DragonRuby's hot-reload works independently of Stargate

### Setup
- Same project from Test 1
- Stargate already infected (modeline shows `🌌`)

### Steps
1. Open any `.rb` file in `mygame/` (or create `test.rb`)
2. Add simple code:
   ```ruby
   puts "Hot-reload test: #{Time.now}"
   ```
3. **Save the file** (`C-x C-s`)
4. Observe DragonRuby console

### Expected Results
✅ **DragonRuby console shows**:
```
[INFO] Reloading changed Ruby files.
Hot-reload test: 2026-01-20 09:30:15 -0500
```

✅ **Emacs doesn't send any commands** (check `*Messages*` buffer - should be silent or only show diagnostics)

✅ **Game continues running at 60 FPS** (no pause, no freeze)

✅ **Modeline stays at `🌌`** (Stargate remains active)

### Failure Modes

❌ **DragonRuby doesn't reload**:
- Check if file is actually saved to disk
- Verify file is inside `mygame/` directory
- Check for syntax errors in the `.rb` file

❌ **Emacs shows warnings about `send-code`**:
- This is expected if old code paths are still active
- Warnings should say "deprecated" clearly
- Functionality should still work via hot-reload

### Pass Criteria
- Code changes appear in game without manual reload
- No Emacs-initiated commands in console
- 60 FPS maintained throughout

---

## Test 3: Close/Reopen Robustness

**Purpose**: Validate that Stargate handles process lifecycle correctly

### Setup
- Same project from Test 2
- Stargate infected and working

### Steps
1. **Close DragonRuby** (quit the game window)
2. Observe Emacs modeline → should change from `🌌` to `💤`
3. Wait 5 seconds
4. **Restart DragonRuby**: `M-x dragonruby-run`
5. Wait for game window to appear
6. Press **F7** again
7. Observe behavior

### Expected Results
✅ **After closing**:
- Modeline changes to `💤` (Searching for Dragon)
- No errors in `*Messages*`
- Session data preserved in `sessions/session.json`

✅ **After restarting**:
- DragonRuby launches normally
- Modeline stays `💤` initially

✅ **After F7**:
- No prompt (loader already in `main.rb`)
- Portal file recreated
- Modeline transitions: `💤` → `⏳` → `🌌`
- `STARGATE::INFECTED` appears in console

✅ **No errors** at any stage

### Failure Modes

❌ **Modeline stays `🌌` after DragonRuby closes**:
- Check if bridge process detection is working
- Should transition to `💤` within 5 seconds

❌ **F7 prompts for loader again**:
- Check if `main.rb` was accidentally reverted
- Loader should persist across restarts

❌ **Infection takes >5 seconds**:
- Check for file system lag
- Verify hot-reload is enabled in DragonRuby

### Pass Criteria
- Clean state transitions at each step
- No manual file editing required
- Session continuity maintained

---

## Test 4: Passive Waiting State (Edge Case)

**Purpose**: Validate that timeout doesn't cause false errors

### Setup
- Project with loader in `main.rb`
- DragonRuby running
- Stargate NOT yet infected

### Steps
1. **Manually delete** `mygame/stargate_portal.rb` (if it exists)
2. Press **F7**
3. **Immediately pause DragonRuby** (if possible, or simulate slow hot-reload)
4. Observe modeline after 5 seconds
5. Observe modeline after 15 seconds

### Expected Results
✅ **At 5 seconds**:
- Modeline shows `⏳` (waiting for hot-reload)
- Tooltip: "Waiting for DragonRuby hot-reload to detect portal file..."
- **No error messages**

✅ **At 15 seconds**:
- Modeline still `⏳` (patient waiting)
- Gentle message in `*Messages*`: 
  ```
  ⏳ STARGATE: Still waiting for hot-reload. Check that main.rb has the auto-loader snippet.
  ```
- **Not an error** - just a reminder

✅ **After resuming DragonRuby**:
- Hot-reload triggers
- `STARGATE::INFECTED` appears
- Modeline → `🌌`

### Failure Modes

❌ **Error message appears**:
- System should never treat waiting as a failure
- Should remain in passive state indefinitely

❌ **State resets or unlocks**:
- Infection request should stay active
- Should not force retry automatically

### Pass Criteria
- No "timeout" errors
- Waiting state is clearly passive, not alarming
- System recovers gracefully when hot-reload triggers

---

## Test 5: Timeline Recording (Integration)

**Purpose**: Validate that moment capture works end-to-end

### Setup
- Stargate infected (modeline `🌌`)
- Simple game code that increments a counter:
  ```ruby
  def tick(args)
    args.state.counter ||= 0
    args.state.counter += 1
    puts "Frame: #{args.state.counter}"
  end
  ```

### Steps
1. Press **F9** (Start recording)
2. Let game run for ~5 seconds (300 frames)
3. Press **F8** (Open timeline)
4. Observe timeline buffer

### Expected Results
✅ **Timeline buffer shows frames**:
```
Frame 001 | prime@1   | hash: abc123 | ...
Frame 002 | prime@2   | hash: def456 | ...
...
Frame 300 | prime@300 | hash: xyz789 | ...
```

✅ **Frames appear in real-time** (auto-scrolling)

✅ **Clicking a frame** shows state inspection

✅ **`sessions/session.json` exists** and contains moment data

### Failure Modes

❌ **Timeline is empty**:
- Check if recording started (`F9` feedback)
- Verify DragonRuby is printing moments to STDOUT
- Check bridge is reading STDOUT correctly

❌ **Frames don't update in real-time**:
- Check if auto-scroll is enabled
- Verify `process-filter` is active

### Pass Criteria
- Frames appear continuously
- State is captured and retrievable
- Session file is valid JSON

---

## Validation Summary

| Test | Purpose | Critical? | Pass Status |
|------|---------|-----------|-------------|
| 1. Minimal Infection | Core ritual | ✅ YES | ⏳ Pending |
| 2. Hot-Reload Natural | Independence | ✅ YES | ⏳ Pending |
| 3. Close/Reopen | Robustness | ✅ YES | ⏳ Pending |
| 4. Passive Waiting | Edge case | ⚠️ NICE | ⏳ Pending |
| 5. Timeline Recording | Integration | ✅ YES | ⏳ Pending |

---

## After All Tests Pass

### Checkpoints
- [ ] All 5 tests completed successfully
- [ ] No unexpected errors in `*Messages*`
- [ ] DragonRuby console shows clean output
- [ ] Session files are valid and portable

### Next Steps

**If all tests pass**:
1. Mark v0.8.1 as stable
2. Update main README with new model
3. Consider Phase 7 (file-based advanced commands)

**If any test fails**:
1. Document the failure mode
2. Add to known issues
3. Create targeted fix
4. Re-run full test suite

---

## Notes for Future Tests

### Phase 7 Validation (When Implemented)
- Test file-based `pause` command
- Test file-based `resume` command
- Test temporary file injection
- Test command file cleanup

### Performance Tests (Optional)
- Measure hot-reload latency (should be <100ms)
- Measure state capture overhead (should be <1ms/frame)
- Verify 60 FPS maintained under recording

---

**Testing Protocol Status**: READY  
**Execute tests in order**: 1 → 2 → 3 → (4) → 5

🧪 **"Surgical validation, not shotgun debugging."** 🔬
