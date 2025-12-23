# Example 3: Sprites and Geometry
# concepts: args.geometry, args.outputs.sprites

def tick args
  # Define a target box
  target = { x: 1000, y: 100, w: 100, h: 100 }

  # Creates a sprite hash that follows the mouse
  player_cursor = {
    x: args.inputs.mouse.x,
    y: args.inputs.mouse.y,
    w: 50,
    h: 50,
    path: 'sprites/square/blue.png'
  }

  # Use DragonRuby's geometry engine to check collision
  if args.geometry.intersect_rect? player_cursor, target
    # If colliding, render target as green
    args.outputs.solids << [target.x, target.y, target.w, target.h, 0, 255, 0]
  else
    # Else, render target as red
    args.outputs.solids << [target.x, target.y, target.w, target.h, 255, 0, 0]
  end

  # Render our cursor sprite
  args.outputs.sprites << player_cursor
end
