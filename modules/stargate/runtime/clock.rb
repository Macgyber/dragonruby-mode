# frozen_string_literal: true

module Stargate
  # The Authority of Time and Branching.
  # This module governs the Laws of Time (II), The Past (III), and Branching (XI).
  module Clock
    @current_branch = "prime" # UUID for the main timeline
    @current_frame  = 0
    @branch_forest   = {}      # Memory of all branch relationships

    class << self
      attr_reader :current_branch, :current_frame

      # The Ceremonial Tick.
      # Enforces the Sacred Order (Seed -> Input -> Inject -> Tick -> Capture).
      def with_frame(seed, inputs)
        # 1. SEED: Sovereignty over RNG (Law XVII).
        Random.begin_frame(seed)

        # 2. INPUT: Application of deterministic values.
        # Inputs.apply(inputs) if defined?(Inputs)

        # 3. Checkpoint for Dead Hand Rollback (Law IX)
        # We capture state BEFORE any potential mutations (Injection or Tick)
        Injection.checkpoint

        begin
          # 4. INJECT: Trial frames for code hot-reloads (Law VI).
          Injection.perform_injections

          # 5. TICK: The simulation step.
          yield if block_given?

          # 6. CAPTURE: Preservation of the Moment (Law V).
          state_packet = State.capture
          Protocol.emit_moment(current_address, state_packet, seed)

          # 7. INCREMENT: Causal progression (Law II).
          @current_frame += 1
          :ok
        rescue => e
          # DEAD HAND ROLLBACK: Revert to the checkpoint
          puts "[STARGATE_ERROR] CLOCK ERROR: #{e.message}"
          puts e.backtrace.join("\n") if $gtk
          Injection.rollback!
          :error
        end
      end

      # Address as branch_id@frame (Law XI)
      def current_address
        "#{@current_branch}@#{@current_frame}"
      end

      # Fork the timeline (branching)
      def branch!(divergence_frame, parent_id = @current_branch)
        # Custom ID Generation (Law XI) - Compatible with DragonRuby
        new_id = "branch_#{(Time.now.to_f * 1000).to_i}_#{rand(1000)}"
        @branch_forest[new_id] = {
          parent: parent_id,
          divergence: divergence_frame,
          head: divergence_frame
        }
        Protocol.emit_branch(new_id, parent_id, divergence_frame)
        
        @current_branch = new_id
        @current_frame = divergence_frame
        new_id
      end

      # Restore a specific moment coordinates and state.
      def restore_moment(branch_id, frame, state_data)
        @current_branch = branch_id
        @current_frame = frame
        
        $gtk.console.log "⏪ Stargate: Restoring state for #{branch_id}@#{frame}"
        State.apply(state_data) 
        :ok
      end

      # Jump to specific coordinates (Internal use or raw jumps)
      def jump_to(branch_id, frame)
        @current_branch = branch_id
        @current_frame = frame
      end
    end
  end
end
