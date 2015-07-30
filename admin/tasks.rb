require 'date'
require 'yaml'
require 'pathname'

require './config'

# Exception class for execution errors
class ExecutionException < StandardError
  attr_accessor :cmd, :output

  def initialize(cmd, output)
    @cmd = cmd
    @output = output
    super("Error running #{cmd}:\n#{output}")
  end
end


# Creates a new full backup
def run_backup
  stamp = Time.now
  bname = "verboice-#{stamp.strftime('%Y%m%dT%H%M%S')}"
  logs_dir = File.expand_path(load_broker_config['record_dir'])

  Dir.chdir(BACKUPS_FOLDER) do
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
  source = File.expand_path(File.join(BACKUPS_FOLDER, name))
  data_dir = File.dirname(File.expand_path(load_broker_config['record_dir']))

  Dir.chdir(data_dir) do
    checked_exec "tar xvzf #{source}"
    restore_db "#{bname}.sql"
  end

  migrate_db
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
  Dir.chdir(RAILS_PATH) { checked_exec "RAILS_ENV=#{ENVIRONMENT} BUNDLE_GEMFILE=#{File.join(RAILS_PATH, 'Gemfile')} bundle exec rake db:migrate --trace " }
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
  YAML.load(File.read(DATABASE_CONFIG_PATH))[ENVIRONMENT]
end

# Load erlang config from broker path
def load_broker_config
  raw = File.read(BROKER_CONFIG_PATH)
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
  output = `#{cmd} 2>&1`
  raise ExecutionException.new(cmd, output) if not $?.success?
  return output
end
