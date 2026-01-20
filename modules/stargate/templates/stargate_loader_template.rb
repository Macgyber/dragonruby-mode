# Stargate Auto-Loader Template
# Copy this snippet to the TOP of your mygame/main.rb file

# ═══════════════════════════════════════════════════════════════
# Stargate Auto-Loader (Sovereign Portal Detector)
# ───────────────────────────────────────────────────────────────
# This enables Emacs to inject the Stargate runtime into your game.
# The portal file is created dynamically when you press F7 in Emacs.
# ───────────────────────────────────────────────────────────────
portal = 'mygame/stargate_portal.rb'
require portal if File.exist?(portal)
# ═══════════════════════════════════════════════════════════════

# Your game code starts here...
