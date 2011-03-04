require(File.expand_path '../../../config/boot.rb', __FILE__)
require(File.expand_path '../../../config/environment.rb', __FILE__)
require(File.expand_path '../../../lib/batphone/lib/fastagi.rb', __FILE__)

class FastAGIServer < FastAGIProtocol
  def agi_post_init
    context = AsteriskAdapter.new self

    p "agi_post_init"

    flow = Flow.new context
    flow.run [
      :answer,
      {:play => 'http://people.sc.fsu.edu/~jburkardt/data/wav/woman.wav'},
      {:puts => 'Sigo...'},
      :hangup,
      {:puts => 'Funciona?'},
    ]
  rescue Exception => e
    puts "#{e.message}: #{e.backtrace}"
  end
end

EM::run do
  EM::start_server '127.0.0.1', 19000, FastAGIServer
end
