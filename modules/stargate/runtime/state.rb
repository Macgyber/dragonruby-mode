# frozen_string_literal: true

module Stargate
  # The Archivist of Matter.
  # Responsibility: Low-level serialization and disk bridge.
  module State
    class << self
      def capture
        raw = $gtk.serialize_state($gtk.args.state)
        return nil if raw.nil? || raw.empty?
        
        hash = calculate_hash(raw)
        # Delegate to BlackBox (Memory-First Law)
        BlackBox.record(hash, raw)
        
        { data: raw, hash: hash }
      rescue => e
        nil
      end

      def calculate_hash(str)
        h = 0
        str.each_byte { |b| h = ((h << 5) - h) + b; h &= 0x7FFFFFFF }
        "h#{str.length}_#{h.to_s(16)}"
      end

      def save_to_disk(hash, data)
        $gtk.write_file(".stargate/blobs/#{hash}", data)
      end

      def load_from_disk(hash)
        $gtk.read_file(".stargate/blobs/#{hash}")
      end

      def apply(raw)
        restored = $gtk.deserialize_state(raw)
        if restored
          $gtk.args.state = restored
          :ok
        else
          :divergent
        end
      end
    end
  end
end
