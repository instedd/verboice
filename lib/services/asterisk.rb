require(File.expand_path '../../../config/boot.rb', __FILE__)
require(File.expand_path '../../../config/environment.rb', __FILE__)
require(File.expand_path '../../../lib/batphone/lib/fastagi.rb', __FILE__)

class FastAGIServer < FastAGIProtocol
  def agi_post_init
    context = AsteriskAdapter.new self

    flow = Flow.new context
    flow.run "#{Rails.root}/lib/services/commands.rb"
  end
end

set_trace_func proc { |event, file, line, id, binding, classname|
  printf "%8s %s:%-2d %10s %8s\n", event, file, line, id, classname if event == 'raise'
}

EM::run do
  EM::start_server '127.0.0.1', 19000, FastAGIServer
end