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
        $gtk.console.log "🌌 STARGATE: Interposing on GTK heartbeat..."
        
        # We define a wrapper that replaces the global 'tick'
        Object.send(:define_method, :stargate_tick_wrapper) do |args|
          Stargate::Bootstrap.tick(args)
        end

        # DragonRuby calls the 'tick' method on the top-level object.
        # We can't easily alias it because it might not be defined yet when we require this.
        # Instead, we tell the user to call Stargate::Bootstrap.tick(args) inside their tick,
        # or we can use a more aggressive approach if they want "automatic" interposition.
        @installed = true
      end

      # The actual wrapper for the DragonRuby tick.
      # Users should call this inside their global 'tick(args)' method.
      def tick(args)
        # For now, we use a simple incrementing seed if none is provided.
        # In full operation, the Chronicler/Mind will provide this.
        frame_seed = (Time.now.to_f * 1000).to_i
        dummy_inputs = args.inputs

        Stargate::Clock.with_frame(frame_seed, dummy_inputs) do
          # We call the user's original logic (which might be renamed or handled here)
          yield if block_given?
        end
      end
    end
  end
end
