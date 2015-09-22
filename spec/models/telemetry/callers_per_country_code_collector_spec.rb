require 'spec_helper'

describe Telemetry::CallersPerCountryCodeCollector do
  
  it "builds counters with callers per country code and project id" do
    p1 = Project.make
    p2 = Project.make
    
    (1..3).each { |i| create_contact p1, "54 11 4444 555#{i}" }
    (4..9).each { |i| create_contact p2, "54 11 4444 555#{i}" }
    (1..4).each { |i| create_contact p1, "855 23 686 036#{i}" }
    (5..7).each { |i| create_contact p2, "855 23 686 036#{i}" }
    
    # unknown country codes
    create_contact p2, "123"
    create_contact p2, "456"

    current_stats["counters"].should =~ [
      {
        "type" => "callers",
        "key" => { "project_id" => p1.id, "country_code" => "54" },
        "value" => 3
      },
      {
        "type" => "callers",
        "key" => { "project_id" => p1.id, "country_code" => "855" },
        "value" => 4
      },
      {
        "type" => "callers",
        "key" => { "project_id" => p2.id, "country_code" => "54" },
        "value" => 6
      },
      {
        "type" => "callers",
        "key" => { "project_id" => p2.id, "country_code" => "855" },
        "value" => 3
      },
      {
        "type" => "callers",
        "key" => { "project_id" => p2.id },
        "value" => 2
      }
    ]
  end

  def create_contact(project, address)
    project.contacts.make addresses_attributes: [{address: address}]
  end

  def current_stats
    period = InsteddTelemetry.current_period
    Telemetry::CallersPerCountryCodeCollector.collect_stats(p)
  end

end
