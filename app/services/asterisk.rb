require(File.expand_path '../../../lib/batphone/lib/fastagi.rb', __FILE__)

class FastAGIServer < FastAGIProtocol
  def agi_post_init
    answer
    p "Hola!"

    stream_file('beep', nil).callback do |response|
      close_connection
    end
  end
end

EM::run do
  EM::start_server '127.0.0.1', 19000, FastAGIServer
end
