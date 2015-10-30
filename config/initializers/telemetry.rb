InsteddTelemetry.setup do |conf|

  custom_config = Rails.configuration.telemetry_configuration rescue {}

  conf.server_url           = custom_config[:server_url]                   if custom_config.include? :server_url
  conf.period_size          = custom_config[:period_size_hours].hours      if custom_config.include? :period_size_hours
  conf.process_run_interval = custom_config[:run_interval_minutes].minutes if custom_config.include? :run_interval_minutes

  conf.remote_api_enabled = true

  # Verboice custom collectors

  conf.add_collector Telemetry::CallersPerCountryCodeCollector
  conf.add_collector Telemetry::CallFlowsPerProjectCollector
  conf.add_collector Telemetry::CallsPerDayPerChannelCollector
  conf.add_collector Telemetry::LanguagesPerProjectCollector
  conf.add_collector Telemetry::ProjectCountCollector
  conf.add_collector Telemetry::StepsPerCallFlowCollector
  conf.add_collector Telemetry::ActiveChannelsCollector

end
