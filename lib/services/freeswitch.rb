require(File.expand_path '../../../config/boot.rb', __FILE__)
require(File.expand_path '../../../config/environment.rb', __FILE__)
require 'librevox'

class MyApp < Librevox::Listener::Outbound
  #event :channel_hangup do
    #done
  #end

  def session_initiated
    pbx = FreeswitchAdapter.new self

    app_id = session[:variable_verbo_application_id]
    app = Application.find app_id
    begin
      app.run pbx
    rescue Exception => ex
      puts "FATAL: #{ex.inspect}"
      close_connection
    ensure
      done
    end
  end
end

Librevox.start MyApp, :port => "9876"
