# frozen_string_literal: true

module Stargate
  # The Sovereignty of Randomness (Law XVII).
  # Responsibility: Provide deterministic PRNG values based on frame seeds.
  module Random
    @prng = nil
    @calls_this_frame = 0

    class << self
      attr_reader :calls_this_frame

      def begin_frame(seed)
        @prng = ::Random.new(seed)
        @calls_this_frame = 0
      end

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
        begin_frame(Time.now.to_i) # Emergency fallback only
      end
    end
  end
end

# Law XVII: Interception must be explicit and minimal.
module Kernel
  def rand(max = 1.0)
    Stargate::Random.rand(max)
  end
end

if $gtk
  class << $gtk
    def rand(max = 1.0)
      Stargate::Random.rand(max)
    end
  end
end
