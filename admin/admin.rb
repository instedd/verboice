require 'rubygems'
require 'sinatra'
require "sinatra/reloader" if development?

require './config'
require './tasks'


# Error handling
set :show_exceptions, :after_handler

error ExecutionException do
  @ex = env['sinatra.error']
  erb :error
end


# Home
get '/' do
  redirect to('/backups')
end


# Success
get '/success/:name' do
  @name = params[:name]
  erb :success
end


# Show config
get '/config' do
  @database = load_dbconfig
  @broker = load_broker_config
  erb :config
end


# List backups
get '/backups' do
  @entries = Dir.glob("#{BACKUPS_FOLDER}/*.tar.gz").map { |entry| entry_info(entry) }
  erb :backups
end


# Create a new backup
post '/backups' do
  run_backup
  redirect to('/backups')
end


# View details of a backup
get '/backups/:path' do
  path = File.join(BACKUPS_FOLDER, params[:path])
  if File.exists?(path)
    contents = get_backup_contents(path)
    info = entry_info(path)

    @path = params[:path]
    @call_logs = contents.select{|line| line[-1] =~ /\d+\/?$/}
    @db_dump_size = contents.find{|line| line[-1] =~ /\.sql$/}[4].to_i
    @locked = info[:locked]
    @time = info[:time]
    erb :show
  else
    erb :notfound
  end
end


# Download backup
get '/download/:path' do
  path = File.join(BACKUPS_FOLDER, params[:path])
  send_file path
end


# Upload a backup file
post '/upload' do
  name = params[:file][:filename]
  target = File.join(BACKUPS_FOLDER, name)
  file = params[:file][:tempfile]
  File.open(target, 'wb') { |f| f.write(file.read) }
  redirect to("/backups/#{name}")
end


# Restore a backup
post '/restore/:path' do
  restore_backup(params[:path])
  redirect to("/success/#{params[:path]}")
end

