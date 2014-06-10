require_relative 'rails'

Hercule::Backend.client = Elasticsearch::Client.new(host: Rails.configuration.verboice_configuration[:poirot_elasticsearch_url])
