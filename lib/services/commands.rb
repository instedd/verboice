# These are the commands that get executed in asterisk.rb and freeswitch.rb ... for now
answer
puts "Playing..."
#play 'http://www.tonycuffe.com/mp3/tailtoddle_lo.mp3'
play 'http://people.sc.fsu.edu/~jburkardt/data/wav/woman.wav'
#puts "Before capture"
#capture :min => 3, :max => 3, :play => 'http://www.tonycuffe.com/mp3/tailtoddle_lo.mp3'
#puts "After capture"
hangup
