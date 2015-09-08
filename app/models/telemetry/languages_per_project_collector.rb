module Telemetry::LanguagesPerProjectCollector

  def self.collect_stats(period)
    sets = []

    Project.select([:id, :languages]).find_each do |project|
      sets << {
        "type" => "languages",
        "key" => { "project_id" => project.id },
        "elements" => project.languages
      }
    end

    { "sets" => sets }
  end

end