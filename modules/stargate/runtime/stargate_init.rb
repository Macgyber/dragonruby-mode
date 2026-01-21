# frozen_string_literal: true

# Stargate Boot Sequencer
# Responsibility: Orchestrate the minimal boot sequence for the runtime.

$stdout.sync = true
puts "STARGATE: [BOOT] Initializing Experts..."

# The Registry of Authority
Stargate::Bootstrap.install!

puts "STARGATE: [BOOT] Success. Systems Online."
puts "{\"type\": \"ready\"}"
$stdout.flush
