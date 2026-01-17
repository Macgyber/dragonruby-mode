# frozen_string_literal: true

module Stargate
  module Clock
    @current_branch = "prime" # UUID for the main timeline
    @current_frame  = 0
    @branch_forest   = {}      # Memory of all branch relationships

    class << self
      attr_reader :current_branch, :current_frame

      def with_frame(seed, inputs)
        if @paused
          Protocol.emit_moment(current_address, { hash: "PAUSED" }, seed, "stasis")
          return :paused
        end

        if @last_authoritative_hash
          current_raw = $gtk.serialize_state
          current_hash = $gtk.sha256(current_raw)
          if current_hash != @last_authoritative_hash
            Protocol.emit_divergence(current_address, @last_authoritative_hash, current_hash)
            pause!
            return :divergence
          end
        end

        Random.begin_frame(seed)

        # Inputs.apply(inputs) if defined?(Inputs)

        Injection.checkpoint

        begin
          Injection.perform_injections

          yield if block_given?

          state_packet = State.capture
          @last_authoritative_hash = state_packet[:hash]
          Protocol.emit_moment(current_address, state_packet, seed)

          @current_frame += 1
          :ok
        rescue => e
          puts "[STARGATE_ERROR] CLOCK ERROR: #{e.message}"
          puts e.backtrace.join("\n") if $gtk
          Injection.rollback!
          :error
        end
      end

      def current_address
        "#{@current_branch}@#{@current_frame}"
      end

      # Fork the timeline (branching)
      def branch!(divergence_frame, parent_id = @current_branch)
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

      def restore_moment(branch_id, frame, hash, seed)
        @current_branch = branch_id
        @current_frame = frame
        
        $gtk.console.log "⏪ Stargate: Restoring state for #{branch_id}@#{frame} (Hash: #{hash})"
        
        data = State.load_from_disk(hash)
        if data
          State.apply(data)
          @last_authoritative_hash = hash
          # Ensure RNG is also restored to this point
          Random.begin_frame(seed)
          :ok
        else
          $gtk.console.log "❌ ERROR: State blob #{hash} not found on disk!"
          :error
        end
      end

      def pause!
        @paused = true
        Random.reset!
        $gtk.console.log "🛑 STARGATE: Simulation PAUSED (Stasis Mode)."
      end

      def resume!
        @paused = false
        $gtk.console.log "▶️ STARGATE: Simulation RESUMED."
      end

      # Jump to specific coordinates (Internal use or raw jumps)
      def jump_to(branch_id, frame)
        @current_branch = branch_id
        @current_frame = frame
      end
    end
  end
end
