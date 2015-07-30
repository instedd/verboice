# Config
@@environment = ENV["RAILS_ENV"] || 'development'
@@backups_folder = ENV["VERBOICE_BACKUPS"] || './backups'
@@database_config_path = File.expand_path("../../config/database.yml", __FILE__)
@@broker_config_path = File.expand_path("../../broker/verboice.config", __FILE__)
@@rails_path = File.expand_path("../..", __FILE__)
