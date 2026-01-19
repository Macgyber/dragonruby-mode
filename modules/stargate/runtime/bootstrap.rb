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
      # Laws of the Machine
      def install!
        return if $stargate_installed
        
        # 1. Detect existing tick (everywhere)
        tick_exists = Object.respond_to?(:tick, true) || 
                      Object.private_instance_methods.include?(:tick) ||
                      Object.instance_methods.include?(:tick)

        # 2. Sovereign Alias Protection (Idempotency)
        # Prevents infinite recursion if re-evaluated during hot reload
        has_alias = Object.private_instance_methods.include?(:__stargate_original_tick__) ||
                    Object.instance_methods.include?(:__stargate_original_tick__)
        
        unless has_alias
          if tick_exists
            Object.send(:alias_method, :__stargate_original_tick__, :tick)
          else
            # Law of the Void: If there's no tick, we provide the canvas
            Object.send(:define_method, :__stargate_original_tick__) { |args| }
          end
        end

        # 3. Interposition
        Object.send(:define_method, :tick) do |args|
          Stargate::Bootstrap.tick(args) { __stargate_original_tick__(args) }
        end

        $stargate_installed = true
        puts "STARGATE: Interposition SUCCESSful."
        # Signal ACK to Emacs Bridge
        puts "{ \"type\": \"infection_ack\" }"
      end

      # High-Level API
      def infect!
        puts "STARGATE: Infection requested..."
        install!
      end

      def engage!
        install!
        $stargate_operational = true
      end

      # The actual wrapper for the DragonRuby tick.
      def tick(args)
        # Law I: Heartbeat (Gated by Debug Flag)
        if $stargate_debug && args.state.tick_count % 60 == 0
          puts "[STARGATE_DEBUG] Heartbeat Pulse: Frame #{args.state.tick_count}" 
        end

        # Law XVII: Continuous Determinism via Seed Locking
        frame_seed = (args.state.tick_count + 1) * 1000
        
        Stargate::Clock.with_frame(frame_seed, args.inputs) do
          # Call the user's original logic (the original tick)
          yield if block_given?
        end
      end
    end
  end
end
