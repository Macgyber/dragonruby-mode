# frozen_string_literal: true

module Stargate
  # The Guardian of Temporal Address.
  module Chronos
    @current_branch = "prime"
    @current_frame  = 0
    @branch_forest   = {}

    class << self
      attr_reader :current_branch, :current_frame

      def set_address(branch, frame)
        @current_branch = branch
        @current_frame = frame
      end

      def advance_frame!
        @current_frame += 1
      end

      def address
        "#{@current_branch}@#{@current_frame}"
      end

      def register_branch(id, parent, divergence)
        @branch_forest[id] = { parent: parent, divergence: divergence, head: divergence }
      end
    end
  end
end
