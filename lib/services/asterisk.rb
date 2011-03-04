require(File.expand_path '../../../config/boot.rb', __FILE__)
require(File.expand_path '../../../config/environment.rb', __FILE__)
require(File.expand_path '../../../lib/batphone/lib/fastagi.rb', __FILE__)

class FastAGIServer < FastAGIProtocol
  def agi_post_init
    context = AsteriskAdapter.new self

    flow = Flow.new context
    flow.run [:answer]

    p "Hola!"

    #stream_file('beep', nil).callback do |response|
      #close_connection
    #end

    close_connection
  rescue Exception => e
    puts "#{e.message}: #{e.backtrace}"
  end
end

EM::run do
  EM::start_server '127.0.0.1', 19000, FastAGIServer
end
