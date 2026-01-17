# Stargate Verification Suite
# This is a sample game designed to test the Laws of Time.

def tick args
  # Initialize State
  args.state.ball_x ||= 100
  args.state.ball_dir ||= 1
  args.state.integrity_check ||= 0
  args.state.rng_x ||= 640
  args.state.rng_y ||= 360
  
  # 1. Linear Movement (State Integrity)
  args.state.ball_x += 5 * args.state.ball_dir
  if args.state.ball_x > 1180 || args.state.ball_x < 100
    args.state.ball_dir *= -1
  end

  # 2. Randomness (Law XVII)
  # Every 60 frames, jump to a random position.
  # If Stargate is working, replaying these frames will result in the EXACT same jumps.
  if args.tick_count % 60 == 0
    args.state.rng_x = rand(1080) + 100
    args.state.rng_y = rand(520) + 100
  end

  # --- RENDER ---

  # Background
  args.outputs.solids << [0, 0, 1280, 720, 20, 20, 30]

  # The Ball (State Test)
  args.outputs.sprites << {
    x: args.state.ball_x,
    y: 360,
    w: 64,
    h: 64,
    path: 'sprites/coin.png'
  }

  # The Spark (RNG Test)
  args.outputs.sprites << {
    x: args.state.rng_x,
    y: args.state.rng_y,
    w: 32,
    h: 32,
    path: 'sprites/player.png'
  }

  # UI / Integrity Check
  args.outputs.labels << [640, 680, "STARGATE VERIFICATION SUITE", 10, 1, 255, 255, 255]
  args.outputs.labels << [640, 640, "Frame: #{args.tick_count}", 5, 1, 200, 200, 200]
  args.outputs.labels << [640, 600, "Integrity Hash variable: #{args.state.integrity_check}", 2, 1, 100, 255, 100]
  
  args.outputs.labels << [20, 100, "TESTS:", 2, 0, 255, 255, 255]
  args.outputs.labels << [20, 80, "1. Replay frames: Ball and Orange Circle must follow same path.", 0, 0, 180, 180, 180]
  args.outputs.labels << [20, 60, "2. Divergence: Run '$args.state.integrity_check += 1' in DragonRuby Console.", 0, 0, 180, 180, 180]
  args.outputs.labels << [20, 40, "3. OS Guardian: Save this file from Notepad or another editor.", 0, 0, 180, 180, 180]
end
