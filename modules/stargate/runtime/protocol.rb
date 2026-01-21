# frozen_string_literal: true

module Stargate
  module Protocol
    class << self
      def emit_moment(address, state_packet, seed, moment_type = 'tick')
        return unless state_packet
        payload = {
          "type" => "moment", "address" => address, "hash" => state_packet[:hash], "seed" => seed,
          "metadata" => {
            "observed_at" => { "tick" => $gtk.args.state.tick_count, "monotonic_ms" => Time.now.to_f * 1000 },
            "rng_calls" => Stargate::Random.calls_this_frame, "moment_type" => moment_type
          }
        }
        # Law I: Absolute Observability. We use a prefix that the Bridge filters.
        # To avoid console "lag", we only emit the raw JSON.
        $stdout.puts "[STARGATE_MOMENT] #{json_encode(payload)}"
      end

      def emit_divergence(address, expected, actual)
        payload = { "type" => "divergence", "address" => address, "expected" => expected, "actual" => actual }
        $stdout.puts "[STARGATE_DIVERGENCE] #{json_encode(payload)}"
      end

      def emit_branch(id, parent, div)
        payload = { "type" => "branch", "id" => id, "parent" => parent, "divergence" => div }
        $stdout.puts "[STARGATE_BRANCH] #{json_encode(payload)}"
      end

      def json_encode(obj)
        case obj
        when Hash then "{#{obj.map{|k,v| "\"#{k}\":#{json_encode(v)}"}.join(",")}}"
        when Array then "[#{obj.map{|v| json_encode(v)}.join(",")}]"
        when String then "\"#{obj.to_s.gsub("\"", "\\\"")}\""
        when NilClass then "null"
        when Numeric, TrueClass, FalseClass then obj.to_s
        else "\"#{obj.to_s.gsub("\"", "\\\"")}\""
        end
      end
    end
  end
end
