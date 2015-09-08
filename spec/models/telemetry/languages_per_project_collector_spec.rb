require 'spec_helper'

describe Telemetry::LanguagesPerProjectCollector do
  
  it "builds a set containing languages for all current projects" do
    p1 = Project.make languages: [:eng, :spa]
    p2 = Project.make languages: [:ger, :afr]
    
    current_stats["sets"].should eq([
      {
      "type" => "languages",
      "key" => {"project_id" => p1.id},
      "elements" => [:eng, :spa]
      },
      {
      "type" => "languages",
      "key" => {"project_id" => p2.id},
      "elements" => [:ger, :afr]
      }
    ])
  end

  def current_stats
    period  = InsteddTelemetry.current_period
    Telemetry::LanguagesPerProjectCollector.collect_stats(p)
  end

end
