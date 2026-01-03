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
  h1 = { r: 25, g: 255, b: 100 }
  
  h2 = {
    r: 100,
    g: 200,
    b: 100
  }

  h3 = { r: 50, g: 0, b: 50 } # Compact

  # --- 2. SPRITES (FASE 2) --------------------------------
  # Valid Sprites (Hover to see preview / path)
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
  args.outputs.sprites << { x: 0, y: 0, w: width, h: height, path: :pixel, r: 0, g: 110, b: 70 }

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

  # Render Text
  args.outputs.labels << [640, 360, "DragonRuby Emacs Mode Test", 1, 1]
end
