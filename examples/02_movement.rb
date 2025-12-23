# Example 2: Movement and State
# concepts: args, args.state, args.inputs, tick

def tick args
  # 1. Initialize State
  # Use ||= used to set initial values only once.
  args.state.player ||= { x: 640, y: 360, w: 50, h: 50, speed: 10 }

  # 2. Input Logic
  # We read inputs directly from args.inputs
  if args.inputs.keyboard.left
    args.state.player.x -= args.state.player.speed
  elsif args.inputs.keyboard.right
    args.state.player.x += args.state.player.speed
  end

  # 3. Render Output
  # Render the player as a red square
  args.outputs.solids << [
    args.state.player.x,
    args.state.player.y,
    args.state.player.w,
    args.state.player.h,
    255, 0, 0
  ]
end
