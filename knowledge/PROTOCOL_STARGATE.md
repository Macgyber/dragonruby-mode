# Stargate Protocol Specification (v1.0)

This document formalizes the communication and storage protocols for Stargate. It serves as the "Sovereign Law" for all future implementations of forks, replays, and cross-system state synchronization.

## 1. Session Schema (The Index)
The `.stargate/session-YYYYMMDD-HHMMSS/session.json` file is the authoritative index of a simulation session.

| Field | Type | Description |
| :--- | :--- | :--- |
| `schema_version` | integer | Must be `1` for this version. |
| `version` | string | SemVer of the emitter (e.g., `"1.0.0"`). |
| `metadata` | object | Global session information. |
| `branches` | object | Map of branch-id to parent and divergence data. |
| `moments` | object | Map of `branch@frame` to state metadata. |
| `branch-maps` | object | Optimized lookup for rendering (Ordered list of moments per branch). |

### Moment Metadata Object
Each moment in `moments` must contain:
- `hash`: SHA-256 of the state.
- `seed`: RNG seed used for the frame.
- `observed_at`: Temporal object `{tick, monotonic_ms}`.
- `moment_type`: One of `prime`, `input`, `event`, `anomaly`.

---

## 2. Event Contracts (Telemetry)
Events emitted by the DragonRuby Bridge MUST follow these formats.

### Type: `moment`
Emitted for every authoritative frame.
```json
{
  "type": "moment",
  "address": "branch@frame",
  "hash": "sha256...",
  "seed": 123456,
  "moment_type": "prime",
  "observed_at": { "tick": 0, "monotonic_ms": 0 }
}
```

### Type: `divergence`
Emitted when non-determinism is detected.
```json
{
  "type": "divergence",
  "address": "branch@frame",
  "expected": "expected-hash",
  "actual": "actual-hash"
}
```

---

## 3. Temporal Semantics
- **Logic Tick**: Monotonic integer starting from 0. Never resets within a branch.
- **Monotonic Time**: Physical wall-clock time in milliseconds. Used for performance auditing, not logic.
- **Ordered Sequence**: Moments in a branch-map MUST be ordered by Tick ascending.

---

## 4. Forward Compatibility
- **Additions**: New fields in objects are permitted and should be ignored by older versions.
- **Breaking Changes**: Changing field types or removing mandatory fields requires a `schema_version` increment.
- **Reserved Keys**: Any key prefixed with `_` is reserved for implementation-specific internal use.

**"Before we expand the universe, we define its physics."** 🌌⚖️🐉🦾💎
