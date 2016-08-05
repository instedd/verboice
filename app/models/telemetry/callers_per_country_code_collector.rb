module Telemetry::CallersPerCountryCodeCollector

  def self.collect_stats(period)
    query = ContactAddress.select(["address", "project_id"])
                          .where("created_at < ?", period.end)
                          .to_sql

    results = ActiveRecord::Base.connection.execute query

    grouped = results.group_by do |address, project_id|
      country_code = InsteddTelemetry::Util.country_code(address) rescue nil
      [country_code, project_id]
    end

    counters = grouped.inject([]) do |ret, group|
      country_code, project_id = group[0]
      total = group[1].length
      key   = { "project_id" => project_id, "country_code" => country_code }.select {|k,v| v.present?}

      ret.push({
        "metric"  => "unique_phone_numbers_by_project_and_country",
        "key"   => key,
        "value" => total
      })
    end

    query = ContactAddress.select("DISTINCT address")
                          .where("created_at < ?", period.end)
                          .to_sql
    results = ActiveRecord::Base.connection.execute query
    grouped = results.group_by do |record|
      address = record[0]
      country_code = InsteddTelemetry::Util.country_code(address) rescue nil
      country_code
    end
    grouped.each do |group|
      country_code = group[0]
      total = group[1].length
      next unless country_code
      counters.push({
        "metric" => "unique_phone_numbers_by_country",
        "key"    => { "country_code" => country_code },
        "value"  => total
      })
    end

    { "counters" => counters }
  end

end
