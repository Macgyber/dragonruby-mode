require_relative "../../modules/stargate/runtime/stargate_init"

def tick args
  Stargate::Bootstrap.tick(args) do
  # ========================================================
  # 🧪 DRAGONRUBY EMACS PLUGIN - VALIDATION SUITE
  # ========================================================

  # --- 1. COLORS (FASE 1) ---------------------------------
  # Arrays (RGB & RGBA, hexadecimal, hash)
  c1 =   [255, 0, 0]
  c2 =   [250, 25, 0, 128]
  h1 = { r: 255, g: 255, b: 100 }
  hex1 = 0xFF80FF
  hex2 = 0x2700FF
   args.outputs.sprites << { x: 0, y: 0, w: width, h: height, path: :pixel, r:100, g: 110, b: 70 }

  # Hexadecimal
  # Hexadecimal Litearals (Ruby Numbers)
  hex1 = 0xFF80FF
  hex2 = 0x2700FF

  # UNSUPPORTED (No coloring expected)
  hex3 = "6b5256" # Hex no string support

  # Symbols (Complete List)
  s1 = :red
  s2 = :green
  s3 = :blue
  s4 = :white
  s5 = :black
  s6 = :gray
  s7 = :indigo
  s8 = :violet
  s9 = :orange
  s10 = :yellow
  s11 = :cyan
  s12 = :magenta

  # Hashes (Single & Multiline)
  h1 = { r: 255, g: 255, b: 100 }
  
  h2 = {
    r: 100,
    g: 210,
    b: 100
  }

  h3 = { r: 150, g: 0, b: 50 } # Compact

  # --- 2. SPRITES (FASE 2: DRAGONS & VISUALS) ------------
  # New Dragon Sprites (Hover to see preview)
  args.outputs.sprites << { x: 200, y: 100, w: 128, h: 128, path: "sprites/dragon_red.png" }
  args.outputs.sprites << { x: 400, y: 100, w: 128, h: 128, path: "sprites/dragon_blue.png" }
  
  # Classic Sprites
  args.outputs.sprites << { x: 0, y: 0, w: 100, h: 100, path: "sprites/potion.png"}
  args.outputs.sprites << { x: 100, y: 0, w: 100, h: 100, path: "sprites/enemy.png" }

  # Missing Sprite (Should be marked as error/red)
  args.outputs.sprites << { path: "sprites/missing.png" }

  # Unsupported Format (Should be marked orange if configured)
  args.outputs.sprites << { path: "sprites/logo.psd" }

  # Solid Pixel (Color + Sprite combined - Testing Granular Highlight)
  # This previously caused the entire hash to be highlighted. Now only r/g/b should be.
  width = 100
  height = 100
  args.outputs.sprites << { x: 0, y: 0, w: width, h: height, path: :pixel, r:100, g: 110, b: 70 }

  # --- 3. NAVIGATION (FASE 3) -----------------------------
  # Ruby requires (clickable paths)
  require "app/conecto.rb"
  require_relative "helpers/utils"
  load "app/tick.rb"

  # Data files (clickable if they exist)
  config = read_file("data/config.json")
  levels = read_file("data/levels.csv")
  text   = read_file("data/dialogue.txt")

  # Non-existent files (should show red underline)
  missing = read_file("data/missing.yml")

  # --- 4. FONTS (FASE 4) ---------------------------------
  # ✅ Valid Fonts (Hover to see live preview icon)
  args.outputs.labels << {
    x: 640, y: 500,
    text: "DragonRuby",
    font: "fonts/title_font.otf",
    size_enum: 20
  }

  args.outputs.labels << {
    x: 640, y: 400,
    text: "Press Start",
    font: "fonts/1.ttf"
  }

  # ❌ Invalid Fonts (Should show RED wave underline / Error tooltip)
  args.outputs.labels << { font: "fonts/ghost_font.ttf" }
  args.outputs.labels << { font: "fonts/unsupported.woff" }

  # --- 5. AUDIO (FASE 5) ---------------------------------
  # ✅ Valid Audio (Hover to see info / Look for 🔊 icon)
  args.outputs.sounds << "sounds/test.ogg"
  args.audio[:sfx] = "sounds/test.wav"

  # ❌ Invalid Audio (Should show RED wave underline)
  args.outputs.sounds << "sounds/missing_track.mp3"
  args.audio[:explosion] = "sounds/error.wav"

  # Render Text
  args.outputs.labels << [640, 360, "DragonRuby Emacs Mode Test", 1, 1]
  args.inputs.controller
  end
end
