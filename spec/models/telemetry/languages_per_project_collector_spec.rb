require 'spec_helper'

describe Telemetry::LanguagesPerProjectCollector do

  it "builds a set containing languages for all current projects" do
    d0 = DateTime.new(2011,1,1,8,0,0)
    d1 = d0 + InsteddTelemetry::Period.span

    Timecop.freeze(d0)
    p1 = Project.make languages: [{'language' => 'eng'}, {'language' => 'spa'}]
    p2 = Project.make languages: [{'language' => 'ger'}, {'language' => 'afr'}]
    period = InsteddTelemetry::Period.current
    
    # project created after period ended
    Timecop.freeze(d1)
    Project.make languages: [:eng]

    stats(period)["sets"].should eq([
      {
      "metric" => "languages",
      "key" => {"project_id" => p1.id},
      "elements" => ["eng", "spa"]
      },
      {
      "metric" => "languages",
      "key" => {"project_id" => p2.id},
      "elements" => ["ger", "afr"]
      }
    ])
  end

  def stats(period)
    Telemetry::LanguagesPerProjectCollector.collect_stats(period)
  end

end
