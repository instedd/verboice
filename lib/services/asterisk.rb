require(File.expand_path '../../../config/boot.rb', __FILE__)
require(File.expand_path '../../../config/environment.rb', __FILE__)
require(File.expand_path '../../../lib/batphone/lib/fastagi.rb', __FILE__)

class FastAGIServer < FastAGIProtocol
  def agi_post_init
    context = AsteriskAdapter.new self

    flow = Flow.new context
    flow.run [
      :answer,
      {:puts => 'Play a gsm'},
      {:play => 'http://www.nch.com.au/acm/sample.gsm'},
      {:puts => 'Play a wav'},
      {:play => 'http://people.sc.fsu.edu/~jburkardt/data/wav/woman.wav'},
      {:puts => 'Play an mp3'},
      {:play => 'http://www.tonycuffe.com/mp3/tailtoddle_lo.mp3'},
      {:puts => 'After play'},
      :hangup,
      {:puts => 'After hangup'},
    ]
  end
end

EM::run do
  EM::start_server '127.0.0.1', 19000, FastAGIServer
end
