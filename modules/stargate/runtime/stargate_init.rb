# frozen_string_literal: true

# Stargate: Sovereign Initiation Script
# Purpose: Activate the Stargate Runtime in a DragonRuby project.

require_relative 'bootstrap'

# Instructions for the Architect:
# 1. require 'modules/stargate/runtime/stargate_init' at the top of your main.rb
# 2. Call Stargate::Bootstrap.tick(args) { your_game_tick(args) } inside your global tick.

puts "🌌 STARGATE: The Machine of Time is Levelled."
puts "  [Status] Body is ready. Mind is watching."

Stargate::Bootstrap.install!
