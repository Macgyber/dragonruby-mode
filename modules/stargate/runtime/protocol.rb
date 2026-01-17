# frozen_string_literal: true

module Stargate
  module Protocol
    class << self
      def emit_moment(address, state_packet, seed, moment_type = 'tick')
        return unless $gtk
        return unless state_packet

        frame_num = address.split('@').last.to_i
        return unless (frame_num % 10 == 0) || moment_type == 'input' || moment_type == 'injection'

        json_payload = "{\"type\":\"moment\",\"address\":\"#{address}\",\"hash\":\"#{state_packet[:hash]}\",\"seed\":#{seed},\"moment_type\":\"#{moment_type}\",\"observed_at\":#{Time.now.to_i}}"

        puts "[STARGATE_MOMENT] #{json_payload}"
      end

      def emit_divergence(address, expected_hash, actual_hash)
        return unless $gtk
        json_payload = "{\"type\":\"divergence\",\"address\":\"#{address}\",\"expected\":\"#{expected_hash}\",\"actual\":\"#{actual_hash}\"}"
        puts "[STARGATE_MOMENT] #{json_payload}"
      end

      def emit_branch(new_id, parent_id, divergence_frame)
        return unless $gtk

        json_payload = "{" \
          "\"type\":\"branch\"," \
          "\"id\":\"#{new_id}\"," \
          "\"parent\":\"#{parent_id}\"," \
          "\"divergence\":#{divergence_frame}" \
          "}"
        puts "[STARGATE_MOMENT] #{json_payload}"
      end
    end
  end
end
