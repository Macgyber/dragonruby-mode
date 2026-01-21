# frozen_string_literal: true

module Stargate
  # Law XIX: Asynchronous Command Bus.
  module Commands
    class << self
      def process!
        cmd_file = "mygame/stargate_cmd.rb"
        return unless $gtk.stat_file(cmd_file) && $gtk.read_file(cmd_file).size > 20

        begin
          puts "STARGATE: [CMD] Executing Order..."
          load(cmd_file)
          # Self-Scrubbing
          $gtk.write_file(cmd_file, "# SCRUBBED AT #{Time.now}\n")
        rescue => e
          puts "STARGATE: [ERROR] CMD REJECTION: #{e.message}"
          $gtk.write_file(cmd_file, "# FAILED: #{e.message}\n")
        end
      end
    end
  end
end
