# These are the commands that get executed in asterisk.rb and freeswitch.rb ... for now
answer
puts "Before capture"
capture :min => 3, :max => 3
puts "After capture"
hangup
