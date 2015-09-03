module Telemetry::LanguagesPerProjectCollector

  def self.collect_stats(period)
    sets = []

    Project.select([:id, :languages]).find_each do |project|
      sets << {
        "type" => "languages",
        "key" => { "project_id" => project.id },
        "value" => project.languages.count
      }
    end

    { "sets" => sets }
  end

end