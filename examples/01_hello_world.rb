# Example 1: Hello World
# concepts: args, args.outputs, tick

def tick args
  # Center text on the screen (720p resolution is standard: 1280x720)
  # [x, y, text, alignment_enum, r, g, b]
  args.outputs.labels << [640, 360, "Hello World from DragonRuby!", 1, 0, 0, 0]
end
