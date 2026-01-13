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

      # Perform the scheduled injections.
      # This is called within the Clock's protected block.
      def perform_injections
        return if @pending_evals.empty?

        @pending_evals.each do |snippet|
          eval(snippet, TOPLEVEL_BINDING)
        end
        @pending_evals.clear
      end

      # Schedule a snippet for the next ritual.
      def schedule(snippet)
        @pending_evals << snippet
      end

      # Restore state to the checkpoint captured before the failed injection.
      def rollback!
        if @trial_state
          $gtk.console.log "⏪ Reverting to pre-injection state..."
          State.apply(@trial_state)
          @trial_state = nil
        end
      end

      private
    end
  end
end
