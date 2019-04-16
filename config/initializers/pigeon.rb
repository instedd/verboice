nuntium_config_path = "#{Rails.root}/config/nuntium.yml"
nuntium_config = File.exists?(nuntium_config_path) ? YAML::load(File.open(nuntium_config_path))[Rails.env] : {}

Pigeon.setup do |config|
  config.application_name = 'Verboice'

  config.nuntium_host = ENV['NUNTIUM_HOST'] || nuntium_config['host']
  config.nuntium_account = ENV['NUNTIUM_ACCOUNT'] || nuntium_config['account']
  config.nuntium_app = ENV['NUNTIUM_APP'] || nuntium_config['application']
  config.nuntium_app_password = ENV['NUNTIUM_APP_PASSWORD'] || nuntium_config['password']

  # If you want to support Nuntium Twitter channels, get your Twitter
  # consumer keys from https://dev.twitter.com/apps
  config.twitter_consumer_key = nuntium_config['twitter_consumer_key']
  config.twitter_consumer_secret = nuntium_config['twitter_consumer_secret']
end
