# frozen_string_literal: true

module Stargate
  # The Memory of the Universe.
  # This module governs State Sovereignty (IV) and Integrity (V).
  module State
    class << self
      # Captures the current canonical state of the simulation.
      # Returns a Hash with :data (serialized string) and :hash (SHA-256).
      def capture
        begin
          # Law V: SHA-256 state tracking
          raw_data = $gtk.serialize_state
          hash = $gtk.sha256(raw_data)
          
          { data: raw_data, hash: hash }
        rescue => e
          $gtk.console.log "ERROR: Stargate State Capture Failed: #{e.message}"
          nil
        end
      end

      # Restores the canonical state from raw serialized data.
      # returns :ok or :divergent (deserialization error).
      def apply(raw_data)
        # Ontological Warning: This only restores data. 
        # Code and assets must be externally aligned.
        
        begin
          # Restore the state into DragonRuby's args.state
          restored = $gtk.deserialize_state(raw_data)
          if restored
            $gtk.args.state = restored
            :ok
          else
            :divergent
          end
        rescue => e
          $gtk.console.log "ERROR: Stargate State Apply Divergence: #{e.message}"
          :divergent
        end
      end
    end
  end
end
