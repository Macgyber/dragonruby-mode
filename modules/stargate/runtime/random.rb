# frozen_string_literal: true

module Stargate
  # The Sovereignty of Randomness.
  # This module governs the Randomness Contract (XVII).
  # All non-deterministic RNG calls are intercepted or replaced.
  module Random
    @prng = nil

    class << self
      # Sovereign entry point for each frame.
      # Controls Law XVII (Randomness).
      def begin_frame(seed)
        @prng = ::Random.new(seed)
      end

      # Deterministic random value.
      def rand(max = 1.0)
        ensure_seeded!
        @prng.rand(max)
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
