# frozen_string_literal: true

module Stargate
  # The Body's Nervous System.
  # Responsibility: Prepend Stargate into DragonRuby's tick cycle.
  module Interposition
    def tick(args)
      Stargate::Clock.tick(args) do
        super(args) rescue nil
      end
    end
  end

  module Bootstrap
    class << self
      def install!
        return if $stargate_installed
        
        unless Object.ancestors.include?(Stargate::Interposition)
          Object.prepend(Stargate::Interposition)
        end

        if verify!
          puts "STARGATE: READY v1.0.2"
          Object.const_set(:STARGATE_INTERPOSED, :verified) rescue nil
          $stargate_installed = true
        end
      end

      def verify!
        Object.ancestors.include?(Stargate::Interposition) && 
        Object.new.method(:tick).owner == Stargate::Interposition rescue false
      end
    end
  end
end
