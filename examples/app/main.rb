def tick args
  # ========================================================
  # 🧪 DRAGONRUBY EMACS PLUGIN - VALIDATION SUITE
  # ========================================================

  # --- 1. COLORS (FASE 1) ---------------------------------
  # Arrays (RGB & RGBA)
  c1 = [255, 0, 0]
  c2 = [0, 255, 0, 128]
  
  # Hexadecimal
  hex1 = 0xFF00FF
  hex2 = 0x0000FF

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
  h1 = { r: 255, g: 255, b: 0 }
  
  h2 = {
    r: 100,
    g: 100,
    b: 100
  }

  h3 = { r: 50, g: 0, b: 50 } # Compact

  # --- 2. SPRITES (FASE 2) --------------------------------
  # Valid Sprites (Hover to see preview / path)
  args.outputs.sprites << { x: 0, y: 0, w: 100, h: 100, path: "sprites/image.png" }
  args.outputs.sprites << { x: 100, y: 0, w: 100, h: 100, path: "sprites/image2.png" }

  # Missing Sprite (Should be marked as error/red)
  args.outputs.sprites << { path: "sprites/missing.png" }

  # Unsupported Format (Should be marked orange if configured)
  args.outputs.sprites << { path: "sprites/logo.psd" }

  # --- 3. NAVIGATION (FASE 3) -----------------------------
  # Click or press RET on the string below to jump to file
  require "app/conecto.rb"

  # Render Text
  args.outputs.labels << [640, 360, "DragonRuby Emacs Mode Test", 1, 1]
end
