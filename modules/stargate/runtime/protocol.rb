# frozen_string_literal: true

module Stargate
  module Protocol
    class << self
      def emit_moment(address, state_packet, seed, moment_type = 'tick')
        return unless $gtk
        return unless state_packet

        # Law of Fidelity: The Protocol DOES NOT filter moments.
        # Every moment is a historical truth.

        payload = {
          type: "moment",
          address: address,
          hash: state_packet[:hash],
          seed: seed,
          metadata: {
            observed_at: {
              tick: $gtk.args.state.tick_count,
              monotonic_ms: Time.now.to_f * 1000
            },
            rng_calls: Stargate::Random.calls_this_frame,
            moment_type: moment_type
          }
        }

        puts "[STARGATE_MOMENT] #{$gtk.to_json(payload)}"
      end

      def emit_divergence(address, expected_hash, actual_hash)
        return unless $gtk
        
        payload = {
          type: "divergence",
          address: address,
          expected: expected_hash,
          actual: actual_hash,
          observed_at: {
            tick: $gtk.args.state.tick_count,
            monotonic_ms: Time.now.to_f * 1000
          }
        }
        
        puts "[STARGATE_DIVERGENCE] #{$gtk.to_json(payload)}"
      end

      def emit_branch(new_id, parent_id, divergence_frame)
        return unless $gtk

        payload = {
          type: "branch",
          id: new_id,
          parent: parent_id,
          divergence: divergence_frame,
          observed_at: {
            tick: $gtk.args.state.tick_count,
            monotonic_ms: Time.now.to_f * 1000
          }
        }
        
        puts "[STARGATE_BRANCH] #{$gtk.to_json(payload)}"
      end
    end
  end
end
