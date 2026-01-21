# frozen_string_literal: true

module Stargate
  # The Navigator of Time Paths.
  module Timeline
    class << self
      def branch!(frame, parent, hash)
        Injection.reset!
        new_id = "branch_#{(Time.now.to_f * 1000).to_i}_#{rand(1000)}"
        
        Chronos.register_branch(new_id, parent, frame)
        Protocol.emit_branch(new_id, parent, frame)
        
        Chronos.set_address(new_id, frame)
        Clock.force_hash!(hash)
        new_id
      end

      def restore!(branch, frame, hash, seed)
        Injection.rollback!
        Injection.reset!
        Clock.force_hash!(nil)

        Chronos.set_address(branch, frame)
        puts "STARGATE: [TIME] Restoring #{branch}@# {frame}"

        # Law of Performance: RAM first, Disk second.
        data = BlackBox.fetch(hash)
        
        if data
          State.apply(data)
          Clock.force_hash!(hash)
          Random.begin_frame(seed)
          :ok
        else
          puts "STARGATE: [ERROR] Blob #{hash} lost in Time!"
          :error
        end
      end
    end
  end
end
