# frozen_string_literal: true

module Stargate
  # The Eternal Memory of the Machine.
  # Responsibility: Store recent history in RAM for zero-latency jumps.
  module BlackBox
    @ram_buffer = {} # hash -> raw_data
    @history_queue = [] # list of hashes (circular)
    @max_frames = 300   # 5 seconds at 60fps

    class << self
      def record(hash, data)
        return if @ram_buffer.key?(hash)

        # Circular Buffer logic
        if @history_queue.size >= @max_frames
          oldest = @history_queue.shift
          @ram_buffer.delete(oldest)
        end

        @ram_buffer[hash] = data
        @history_queue << hash
        
        # Law XII: Persistence is asynchronous and buffered
        # We only write to disk if it's the 60th frame of the second
        if $gtk.args.state.tick_count % 60 == 0
          State.save_to_disk(hash, data)
        end
      end

      def fetch(hash)
        @ram_buffer[hash] || State.load_from_disk(hash)
      end

      def clear!
        @ram_buffer.clear
        @history_queue.clear
      end
    end
  end
end
