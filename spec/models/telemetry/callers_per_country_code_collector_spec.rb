require 'spec_helper'

describe Telemetry::CallersPerCountryCodeCollector do

  it "builds counters with callers per country code and project id" do
    d0 = DateTime.new(2011,1,1,8,0,0)
    d1 = d0 + InsteddTelemetry::Period.span

    Timecop.freeze(d0)

    p1 = Project.make
    p2 = Project.make

    (1..3).each { |i| create_contact p1, "54 11 4444 555#{i}" }
    (4..9).each { |i| create_contact p2, "54 11 4444 555#{i}" }
    (1..6).each { |i| create_contact p1, "855 23 686 036#{i}" }
    (5..7).each { |i| create_contact p2, "855 23 686 036#{i}" }

    # unknown country codes
    create_contact p2, "123"
    create_contact p2, "456"

    period = InsteddTelemetry::Period.current

    # contacts created after period
    Timecop.freeze(d1)
    create_contact p1, "54 11 4666 6666"

    stats(period)["counters"].should =~ [
      {
        "metric" => "unique_phone_numbers_by_project_and_country",
        "key" => { "project_id" => p1.id, "country_code" => "54" },
        "value" => 3
      },
      {
        "metric" => "unique_phone_numbers_by_project_and_country",
        "key" => { "project_id" => p1.id, "country_code" => "855" },
        "value" => 6
      },
      {
        "metric" => "unique_phone_numbers_by_project_and_country",
        "key" => { "project_id" => p2.id, "country_code" => "54" },
        "value" => 6
      },
      {
        "metric" => "unique_phone_numbers_by_project_and_country",
        "key" => { "project_id" => p2.id, "country_code" => "855" },
        "value" => 3
      },
      {
        "metric" => "unique_phone_numbers_by_project_and_country",
        "key" => { "project_id" => p2.id },
        "value" => 2
      },
      {
        "metric" => "unique_phone_numbers_by_country",
        "key" => { "country_code" => "54" },
        "value" => 9
      },
      {
        "metric" => "unique_phone_numbers_by_country",
        "key" => { "country_code" => "855" },
        "value" => 7
      }
    ]
  end

  def create_contact(project, address)
    project.contacts.make addresses_attributes: [{address: address}]
  end

  def stats(period)
    Telemetry::CallersPerCountryCodeCollector.collect_stats(period)
  end

end
