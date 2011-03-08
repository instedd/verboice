require(File.expand_path '../../../config/boot.rb', __FILE__)
require(File.expand_path '../../../config/environment.rb', __FILE__)
require 'librevox'

class MyApp < Librevox::Listener::Outbound
  event :channel_hangup do
    done
  end

  def session_initiated
    context = FreeswitchAdapter.new self

    flow = Flow.new context
    flow.run do
      answer
      puts 'Play a gsm'
      play 'http://www.nch.com.au/acm/sample.gsm'
      puts 'Play a wav'
      play 'http://people.sc.fsu.edu/~jburkardt/data/wav/woman.wav'
      puts 'Play an mp3'
      play 'http://www.tonycuffe.com/mp3/tailtoddle_lo.mp3'
      puts 'After play'
      hangup
      puts 'After hangup'
    end
  end
end

Librevox.start MyApp, :port => "9876"
