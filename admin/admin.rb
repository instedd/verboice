require 'rubygems'
require 'sinatra'
require 'date'
require 'yaml'

# Config
environment = ENV["RAILS_ENV"] || 'development'
backups_folder = ARGV[1] || './backups'
database_config = File.expand_path("../../config/database.yml", __FILE__)

# List backups
get '/backups' do
  @entries = Dir.glob("#{backups_folder}/*.sql.gz").map do |entry|
    {
      path: entry,
      locked: File.exists?(entry.gsub(/\.sql\.gz$/, '.lck')),
      time: DateTime.parse(File.basename(entry, '.sql.gz') =~ /^verboice-([0-9T]+)/ && $1).to_time
    }
  end
  erb :backups
end

# Create a new backup
post '/backups' do
  stamp = Time.now
  bname = File.join(backups_folder, "verboice-#{stamp.strftime('%Y%m%dT%H%M%S')}")

  File.write("#{bname}.lck", 1)
  Thread.new do
    dbconfig = load_dbconfig(database_config, environment)
    backupdb(dbconfig, "#{bname}.sql.gz")
    File.delete("#{bname}.lck")
  end

  sleep(1)
  redirect to('/backups')
end

# Perform actual backup
def backupdb(dbconfig, target)
  user = "-u#{dbconfig['username']}"
  pass = dbconfig['password'] ? "-p#{dbconfig['password']}" : ""
  dbname = dbconfig['database']
  cmd = "mysqldump #{user} #{pass} #{dbname} | gzip > #{target}"
  system(cmd)
end

# Load dbconfig from rails path
def load_dbconfig(database_config, environment)
  YAML.load(File.read(database_config))[environment]
end
