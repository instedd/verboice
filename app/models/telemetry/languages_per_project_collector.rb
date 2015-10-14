module Telemetry::LanguagesPerProjectCollector

  def self.collect_stats(period)
    sets = []

    projects = Project.select([:id, :languages])
                      .where('created_at < ?', period.end)

    projects.find_each do |project|
      sets << {
        "metric" => 'languages',
        "key" => { 'project_id' => project.id },
        "elements" => project.languages.map { |l| l['language']}
      }
    end

    { "sets" => sets }
  end

end
