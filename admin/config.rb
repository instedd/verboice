# Config
ENVIRONMENT = ENV["RAILS_ENV"] || 'development'
BACKUPS_FOLDER = ENV["VERBOICE_BACKUPS"] || './backups'
DATABASE_CONFIG_PATH = File.expand_path("../../config/database.yml", __FILE__)
BROKER_CONFIG_PATH = File.expand_path("../../broker/verboice.config", __FILE__)
RAILS_PATH = File.expand_path("../..", __FILE__)
