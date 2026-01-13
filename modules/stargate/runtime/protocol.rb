# frozen_string_literal: true

module Stargate
  # The Word (Communication Layer).
  # This module governs how the Body (Ruby) speaks to the Mind (Emacs).
  module Protocol
    class << self
      # Sovereign entry point for frame metadata emission.
      # Transmits via DragonRuby Console for Emacs interception.
      def emit_moment(address, state_packet, seed)
        return unless defined?($gtk)

        # Build JSON manually for maximum compatibility and protocol safety.
        # We escape the raw state data string and compact it to a single line.
        data_json = state_packet[:data].gsub(/\n/, ' ').gsub(/\s+/, ' ').gsub('"', '\"')
        
        json_payload = "{\"type\":\"moment\",\"address\":\"#{address}\",\"hash\":\"#{state_packet[:hash]}\",\"seed\":#{seed},\"data\":\"#{data_json}\",\"observed_at\":#{Time.now.to_i}}"

        # [STARGATE_MOMENT] is for the Emacs Chronicler.
        $gtk.console.log "[STARGATE_MOMENT] #{json_payload}"
      end

      def emit_branch(new_id, parent_id, divergence_frame)
        return unless defined?($gtk)

        json_payload = "{" \
          "\"type\":\"branch\"," \
          "\"id\":\"#{new_id}\"," \
          "\"parent\":\"#{parent_id}\"," \
          "\"divergence\":#{divergence_frame}" \
          "}"
        $gtk.console.log "[STARGATE_MOMENT] #{json_payload}"
      end
    end
  end
end
