credentials_path = "#{Rails.root}/config/credentials.yml"
credentials = File.exists?(credentials_path) ? YAML::load(File.open(credentials_path))[Rails.env] : {}

ENCRYPTION_KEY = credentials["attr_encrypted"].try(:[], "encryption_key") || ENV["ATTR_ENCRYPTED_ENCRYPTION_KEY"]
