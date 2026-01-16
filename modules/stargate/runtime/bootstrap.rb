# Stargate: The Body's Nervous System
# This file handles the interposition of the Stargate Runtime over the DragonRuby engine.

require_relative 'clock'
require_relative 'random'
require_relative 'protocol'
require_relative 'state'
require_relative 'injection'

module Stargate
  module Bootstrap
    class << self
      # Interpose Stargate onto the global tick function.
      def install!
        return if @installed
        puts "🌌 STARGATE: Interposing on GTK heartbeat..."
        
        # We define a wrapper that replaces the global 'tick'
        @installed = true
      end

      # The actual wrapper for the DragonRuby tick.
      # Users should call this inside their global 'tick(args)' method.
      def tick(args)
        # HEARTBEAT DIAGNOSTIC (Law I)
        puts "[STARGATE_DEBUG] Heartbeat Pulse: Frame #{args.state.tick_count}" if args.state.tick_count % 60 == 0

        # Law XVII: Continuous Determinism.
        frame_seed = (args.state.tick_count + 1) * 1000
        
        Stargate::Clock.with_frame(frame_seed, args.inputs) do
          # We call the user's original logic
          yield if block_given?
        end
      end
    end
  end
end
