# frozen_string_literal: true

module Stargate
  # The Surgeon of Code.
  # This module governs Code Mutation (VI) and the Dead Hand Rollback (IX).
  module Injection
    @trial_state = nil
    @pending_evals = []

    class << self
      # Capture a state checkpoint for potential rollback.
      def checkpoint
        @trial_state = State.capture
      end

      # Perform the scheduled injections for the CURRENT frame only.
      # This is called within the Clock's protected block.
      def perform_injections
        address = Clock.current_address
        
        # Law of Isolation: Only execute what belongs to this moment.
        # This prevents 'Time Pollution' after a jump/restore.
        @pending_evals.each do |job|
          if job[:address] == address
            eval(job[:code], TOPLEVEL_BINDING)
            job[:executed] = true
          end
        end
        
        # Cleanup executed or stale jobs
        @pending_evals.delete_if { |job| job[:executed] || job[:address] != address }
      end

      # Schedule a snippet for the next ritual.
      # Sovereign Law: Every intention must be anchored to an address.
      def schedule(snippet)
        @pending_evals << { 
          address: Clock.current_address, 
          code: snippet,
          executed: false 
        }
      end

      # Restore state to the checkpoint captured before the failed injection.
      def rollback!
        if @trial_state
          $gtk.console.log "⏪ Reverting to pre-injection speculative state..."
          # Fix: @trial_state is a Hash {data: ..., hash: ...}, apply takes raw data.
          State.apply(@trial_state[:data])
          @trial_state = nil
        end
      end

      # Purge all intentions. Used during time-travel.
      def reset!
        @pending_evals.clear
        @trial_state = nil
      end

      private
    end
  end
end
