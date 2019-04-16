hub_config = {}

if enabled = ENV["HUB_ENABLED"]
  hub_config["enabled"] = enabled == "true"
end

if url = ENV["HUB_URL"]
  hub_config["url"] = url
end

if token = ENV["HUB_TOKEN"]
  hub_config["token"] = token
end

if connector_guid = ENV["HUB_CONNECTOR_GUID"]
  hub_config["connector_guid"] = connector_guid
end

HubClient.current hub_config
