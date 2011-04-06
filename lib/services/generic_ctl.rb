def run(script, id = nil) 
  require 'rubygems'
  require 'daemons'
  app_name = script
  app_name += ".#{id}" if id 
  Daemons.run(
    File.join(File.dirname(__FILE__), "#{script}.rb"),
    :app_name => app_name,
    :dir_mode => :normal,
    :dir => File.join(File.dirname(__FILE__), '..', '..', 'tmp', 'pids'),
    :backtrace => true
  )
end
