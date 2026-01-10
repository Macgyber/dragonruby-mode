# Contract Validation Proof (v0.1)
# Demonstration of friction-less autocompletion in a real game loop.

def tick args
  # 1. State Initialization (Explicit Contract)
  args.state.score ||= 0
  args.state.lives ||= 3

  # 2. Input Handling (Navigable via Contract)
  if args.inputs.keyboard.key_down.space
    args.state.lives -= 1
    args.audio.play = { path: "sounds/jump.wav" }
  end

  # Controller Support (Scalable hierarchy)
  if args.inputs.controller.one.a
    args.state.score += 10
  end

  # 3. Output Rendering (Deep properties)
  # Background
  args.outputs.solids << {
    x: 0,
    y: 0,
    w: args.grid.w,
    h: args.grid.h,
    r: 0,
    g: 0,
    b: 0
  }

  # Player Sprite
  args.outputs.sprites << {
    x: args.grid.center_x,
    y: args.grid.center_y,
    w: 50,
    h: 50,
    path: "sprites/player.png",
    angle: args.state.tick_count
  }

  # UI / HUD
  args.outputs.borders << {
    x: 10,
    y: 10,
    w: 200,
    h: 50,
    r: 255, 
    g: 255, 
    b: 255
  }
end
