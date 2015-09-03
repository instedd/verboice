InsteddTelemetry.setup do |conf|
 
  conf.server_url = "http://localhost:3001"

  conf.add_collector Telemetry::ProjectCountCollector
  conf.add_collector Telemetry::CallFlowsPerProjectCollector
  conf.add_collector Telemetry::LanguagesPerProjectCollector
  conf.add_collector Telemetry::StepsPerCallFlowCollector

end