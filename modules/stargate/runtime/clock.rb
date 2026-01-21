# frozen_string_literal: true

module Stargate
  module Clock
    @last_authoritative_hash = nil
    @paused = false

    class << self
      def force_hash!(h); @last_authoritative_hash = h; end

      def tick(args)
        # Law XVII: Seed Locking
        frame_seed = (args.state.tick_count + 1) * 1000
        
        Commands.process!
        
        with_frame(frame_seed, args.inputs) do
          yield if block_given?
        end
      end

      def with_frame(seed, inputs)
        if @paused
          Protocol.emit_moment(Chronos.address, { hash: "PAUSED" }, seed, "stasis")
          return :paused
        end

        # Law of Divergence
        if @last_authoritative_hash
          current_raw = $gtk.serialize_state
          current_hash = $gtk.respond_to?(:sha256) ? $gtk.sha256(current_raw) : State.calculate_hash(current_raw)

          if current_hash != @last_authoritative_hash
            Protocol.emit_divergence(Chronos.address, @last_authoritative_hash, current_hash)
            pause!
            return :divergence
          end
        end

        Random.begin_frame(seed)
        Injection.checkpoint

        begin
          Injection.perform_injections
          yield if block_given?
          
          Chronos.advance_frame!
          
          state_packet = State.capture
          if state_packet
            @last_authoritative_hash = state_packet[:hash]
            Protocol.emit_moment(Chronos.address, state_packet, seed)
          end
          :ok
        rescue => e
          puts "STARGATE: [CRITICAL] #{e.message}"
          Injection.rollback!
          :error
        end
      end

      def pause!
        @paused = true
        Random.reset!
        $gtk.console.log "🛑 STARGATE: PAUSED"
      end

      def resume!
        @paused = false
        $gtk.console.log "▶️ STARGATE: RESUMED"
      end
    end
  end
end
