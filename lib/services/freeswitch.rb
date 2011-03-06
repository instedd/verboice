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
    flow.run [
      :answer,
      {:play => 'http://people.sc.fsu.edu/~jburkardt/data/wav/woman.wav'},
      {:puts => 'After play'},
      :hangup,
      {:puts => 'After hangup'},
    ]
  end
end

Librevox.start MyApp, :port => "9876"
