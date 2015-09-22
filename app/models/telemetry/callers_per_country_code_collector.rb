module Telemetry::CallersPerCountryCodeCollector

  def self.collect_stats(period)
    results = ActiveRecord::Base.connection.execute <<-SQL
      SELECT address, project_id
      FROM contact_addresses
    SQL

    grouped = results.group_by do |address, project_id|
      country_code = InsteddTelemetry::Util.country_code(address) rescue nil
      [country_code, project_id]
    end

    counters = grouped.inject([]) do |ret, group|
      country_code, project_id = group[0]
      total = group[1].length
      key   = { "project_id" => project_id, "country_code" => country_code }.select {|k,v| v.present?}

      ret.push({
        "type"  => "callers",
        "key"   => key,
        "value" => total
      })
    end

    { "counters" => counters }
  end

end