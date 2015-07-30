require 'rubygems'
require 'sinatra'
require 'date'
require 'yaml'
require "sinatra/reloader" if development?


# Config
@@environment = ENV["RAILS_ENV"] || 'development'
@@backups_folder = ARGV[1] || './backups'
@@database_config_path = File.expand_path("../../config/database.yml", __FILE__)
@@broker_config_path = File.expand_path("../../broker/verboice.config", __FILE__)
@@rails_path = File.expand_path("../..", __FILE__)


# Error handling
set :show_exceptions, :after_handler

class ExecutionException < StandardError
  attr_accessor :cmd, :output

  def initialize(cmd, output)
    @cmd = cmd
    @output = output
    super("Error running #{cmd}:\n#{output}")
  end
end

error ExecutionException do
  env['sinatra.error'].message
end


# Home
get '/' do
  redirect to('/backups')
end


# Show config
get '/config' do
  @database = load_dbconfig
  @broker = load_broker_config
  erb :config
end


# List backups
get '/backups' do
  @entries = Dir.glob("#{@@backups_folder}/*.tar.gz").map { |entry| entry_info(entry) }
  erb :backups
end


# Create a new backup
post '/backups' do
  run_backup
  redirect to('/backups')
end


# View details of a backup
get '/backups/:path' do
  path = File.join(@@backups_folder, params[:path])
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
  path = File.join(@@backups_folder, params[:path])
  send_file path
end


# Upload a backup file
post '/upload' do
  name = params[:file][:filename]
  target = File.join(@@backups_folder, name)
  file = params[:file][:tempfile]
  File.open(target, 'wb') { |f| f.write(file.read) }
  redirect to("/backups/#{name}")
end


# Restore a backup
post '/restore/:path' do
  restore_backup(params[:path])
end


# Creates a new full backup
def run_backup
  stamp = Time.now
  bname = "verboice-#{stamp.strftime('%Y%m%dT%H%M%S')}"
  logs_dir = File.expand_path(load_broker_config['record_dir'])

  Dir.chdir(@@backups_folder) do
    File.write("#{bname}.lck", 1)
    backup_call_logs("#{bname}.tar", logs_dir)
    backup_db("#{bname}.sql")
    checked_exec "tar rf #{bname}.tar #{bname}.sql" # Append to tarfile
    checked_exec "gzip #{bname}.tar" # Compress
    File.delete("#{bname}.sql")
    File.delete("#{bname}.lck")
  end
end

# Restores a full backup given its full path
def restore_backup(name)
  bname = name.gsub(/\.tar\.gz$/, '')
  source = File.expand_path(File.join(@@backups_folder, name))
  data_dir = File.dirname(File.expand_path(load_broker_config['record_dir']))

  Dir.chdir(data_dir) do
    checked_exec "tar xvzf #{source}"
    restore_db "#{bname}.sql"
  end

  migrate_db
  redirect to("/backups/#{name}")
end

# Inspects contents of backup
def get_backup_contents(target)
  checked_exec("tar tvzf #{target}").split("\n").map{|line| line.split(/\s+/)}
end

# Backup sounds directories
def backup_call_logs(target, logs_dir)
  logs_dir = Pathname.new(logs_dir)
  cmd = "(cd #{logs_dir.dirname} && tar cf #{File.expand_path(target)} #{logs_dir.basename})"
  checked_exec(cmd)
end

# Restore call logs
def restore_call_logs(source)
  dir = Pathname.new(File.expand_path(load_broker_config['record_dir']))
  cmd = "(cd #{dir.dirname} && tar xf #{File.expand_path(source)} --wildcards --no-anchored 'call_logs*')"
  checked_exec(cmd)
end

# Restore DB from sql gz file
def restore_db(source)
  dbconfig = load_dbconfig
  user = "-u#{dbconfig['username']}"
  pass = dbconfig['password'] ? "-p#{dbconfig['password']}" : ""
  dbname = dbconfig['database']
  checked_exec "(mysql #{user} #{pass} -e\"DROP DATABASE #{dbname}; CREATE DATABASE #{dbname};\") && (mysql #{user} #{pass} #{dbname} < #{source})"
  File.delete source
end

# Invokes rails db:migrate command
def migrate_db
  Dir.chdir(@@rails_path) { checked_exec "RAILS_ENV=#{@@environment} BUNDLE_GEMFILE=#{File.join(@@rails_path, 'Gemfile')} bundle exec rake db:migrate --trace " }
  # Dir.chdir(@@rails_path) { checked_exec "echo #{@@rails_path} && bundle exec rake db:migrate --trace RAILS_ENV=#{@@environment}" }
  # Dir.chdir(@@rails_path) { checked_exec "bundle exec ruby filer.rb && sfljhsdf" }
  # Dir.chdir(@@rails_path) { checked_exec "bundle exec rake db:migrate --trace RAILS_ENV=#{@@environment}" }
  # checked_exec "(cd #{@@rails_path} && bundle exec rake db:migrate --trace RAILS_ENV=#{@@environment})"
end

# Perform actual db backup
def backup_db(target)
  dbconfig = load_dbconfig
  user = "-u#{dbconfig['username']}"
  pass = dbconfig['password'] ? "-p#{dbconfig['password']}" : ""
  dbname = dbconfig['database']
  cmd = "mysqldump #{user} #{pass} #{dbname} > #{target}"
  checked_exec(cmd)
end

# Load dbconfig from rails path
def load_dbconfig
  YAML.load(File.read(@@database_config_path))[@@environment]
end

# Load erlang config from broker path
def load_broker_config
  raw = File.read(@@broker_config_path)
  return {
    'record_dir' => extract_erl_config(raw, 'record_dir'),
    'asterisk_sounds_dir' => extract_erl_config(raw, 'asterisk_sounds_dir')
  }
end

# Extract item from erlang config in the most error-prone way
def extract_erl_config(raw, item)
  raw =~ /\{\s*#{item}\s*,\s*"([^"]+)"\s*\}/
  $1
end

# Basic info on a backup entry
def entry_info(entry)
  {
    path: Pathname.new(entry).basename,
    locked: File.exists?(entry.gsub(/\.tar\.gz$/, '.lck')),
    time: (Time.parse(File.basename(entry, '.tar.gz') =~ /^verboice-([0-9T]+)/ && $1) rescue "Unknown")
  }
end

# Runs a command and raises if it failed
def checked_exec(cmd)
  # output = `#{cmd} 2>&1`
  output = `#{cmd} 2>&1`
  raise ExecutionException.new(cmd, output) if not $?.success?
  return output
end

def checked_system(cmd)
  raise ExecutionException.new(cmd, "") if not system(cmd)
  return true
end
