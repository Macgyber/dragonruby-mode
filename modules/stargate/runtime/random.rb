# frozen_string_literal: true

module Stargate
  # The Sovereignty of Randomness.
  # This module governs the Randomness Contract (XVII).
  # All non-deterministic RNG calls are intercepted or replaced.
  module Random
    @prng = nil
    @calls_this_frame = 0

    class << self
      attr_reader :calls_this_frame

      # Sovereign entry point for each frame.
      # Controls Law XVII (Randomness).
      def begin_frame(seed)
        @prng = ::Random.new(seed)
        @calls_this_frame = 0
      end

      # Deterministic random value.
      def rand(max = 1.0)
        ensure_seeded!
        @calls_this_frame += 1
        @prng.rand(max)
      end

      def reset!
        @prng = nil
        @calls_this_frame = 0
      end

      private

      def ensure_seeded!
        return if @prng
        # Law XVII: $gtk.rand and Kernel.rand must never drift.
        raise "CRITICAL: Randomness Law violated. RNG called before seeding for frame."
      end
    end
  end
end

# INTERCEPTION: Hot-patching DragonRuby and Ruby Kernel.
# Warning: This is a surgical operation required by Law XVII.
module Kernel
  def rand(max = 1.0)
    Stargate::Random.rand(max)
  end
end

if $gtk
  # Intercepting DragonRuby's GTK random interface
  class << $gtk
    def rand(max = 1.0)
      Stargate::Random.rand(max)
    end
  end
end
