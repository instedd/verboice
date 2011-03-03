require 'rubygems'
require 'librevox'

class MyApp < Librevox::Listener::Outbound
  event :channel_hangup do
    done
  end

  def session_initiated
    answer
    p session
    hangup
  end
end

Librevox.start MyApp, :port => "9876"
