# frozen_string_literal: true

module Stargate
  # The Surgeon of Code (Law VI & IX).
  # Responsibility: Manage code injections and state rollbacks for speculative execution.
  module Injection
    @trial_state = nil
    @pending_evals = []

    class << self
      def checkpoint
        @trial_state = State.capture
      end

      def perform_injections
        address = Chronos.address
        @pending_evals.each do |job|
          if job[:address] == address
            eval(job[:code], TOPLEVEL_BINDING)
            job[:executed] = true
          end
        end
        @pending_evals.delete_if { |job| job[:executed] || job[:address] != address }
      end

      def schedule(snippet)
        @pending_evals << { address: Chronos.address, code: snippet, executed: false }
      end

      def rollback!
        if @trial_state && @trial_state.is_a?(Hash) && @trial_state[:data]
          State.apply(@trial_state[:data])
          @trial_state = nil
        end
      end

      def reset!
        @pending_evals.clear
        @trial_state = nil
      end
    end
  end
end
