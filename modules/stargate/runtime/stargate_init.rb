# frozen_string_literal: true
$stdout.sync = true
puts "STARGATE: [BOOT] Initializing Sovereign Runtime..."

# Stargate: Sovereign Initiation Script
# Purpose: Activate the Stargate Runtime in a DragonRuby project.

puts "STARGATE: [TRACE] Protocol loading..."
require_relative 'bootstrap'

puts "STARGATE: [TRACE] Asserting Authority (Prepend)..."
Stargate::Bootstrap.install!

puts "STARGATE: [BOOT] Law of Absolute Randomness ACTIVE."
puts "STARGATE: [BOOT] Law of Irreversible Code ACTIVE."

puts "STARGATE: [TRACE] Initializer sequence complete."
$stdout.flush
