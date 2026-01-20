# Stargate: The Body's Nervous System
# This file handles the interposition of the Stargate Runtime over the DragonRuby engine.

require_relative 'clock'
require_relative 'random'
require_relative 'protocol'
require_relative 'state'
require_relative 'injection'

module Stargate
  module Interposition
    # Law of Precedence: We live BEFORE Object in the lookup chain.
    # We catch every 'tick' and call super (which finds it in Object/Kernel).
    def tick(args)
      Stargate::Clock.tick(args) do
        # Law of Grace: Only call super if a definition exists beyond our module.
        super(args) rescue nil
      end
    end
  end

  module Bootstrap
    class << self
      # Laws of the Machine
      def install!
        puts "STARGATE: [TRACE] Entering install! (installed=#{$stargate_installed})"
        return if $stargate_installed
        
        # 1. AUTHORITY: Prepend our interposition module to Object
        # This puts us at the head of the chain (Object.ancestors[0]).
        unless Object.ancestors.include?(Stargate::Interposition)
          puts "STARGATE: [TRACE] Prepending Interposition to Object..."
          Object.prepend(Stargate::Interposition)
        end

        # 2. PROOF: Verify absolute authority via ancestry
        if verify_interposition
          puts "STARGATE::INFECTED"
          # Final Sentinel (Symbol based, not string)
          Object.const_set(:STARGATE_INTERPOSED, :verified) rescue nil
          puts "STARGATE: [TRACE] Sovereignty Established."
        else
          puts "STARGATE: [ERROR] Interposition failed Ancestry Check."
        end

        $stargate_installed = true
        $stdout.flush
      end

      # The Canonical Proof: Structural, not procedure-based.
      def verify_interposition
        # In Ruby, prepend places the module BEFORE the class in ancestors.
        # But for 'Object', ancestors[0] should be our module.
        ours = Object.ancestors.include?(Stargate::Interposition) && 
               Object.new.method(:tick).owner == Stargate::Interposition rescue false
        
        puts "STARGATE: [TRACE] Verifying Ancestry... ours=#{ours}"
        ours
      end

      # High-Level API
      def infect!
        install!
      end

      def engage!
        install!
        $stargate_operational = true
      end
    end
  end
end
