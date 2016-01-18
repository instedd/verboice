namespace :call_log do
  desc "Import call log details from Poirot"
  task :import_from_poirot => :environment do
    client = Elasticsearch::Client.new
    poirot_indices = client.cat.indices(format: 'json', h: 'i', index: 'poirot-*').map { |x| x["i"] }

    CallLog.includes(:entries).find_each(batch_size: 50) do |call_log|
      next if call_log.entries.any? { |x| x.details.has_key?(:activity) }
      next unless call_log.started_at

      puts "Importing details for call #{call_log.id}"

      indices = [
        call_log.started_at.strftime("poirot-%Y.%m.%d"),
        (call_log.started_at + 1.day).strftime("poirot-%Y.%m.%d"),
        call_log.started_at.strftime("poirot-archive-%Y.%m"),
        (call_log.started_at + 1.day).strftime("poirot-archive-%Y.%m")
      ] & poirot_indices

      if indices.empty?
        puts "--- No indexes found"
        next
      end

      puts "--- Searching indexes: #{indices.join ','}"

      response = client.search index: indices.join(","),
                               q: "call_log_id:#{call_log.id} AND _exists_:step_type",
                               sort: "@start",
                               size: 1000000

      puts "--- Found #{response["hits"]["hits"].length} entries"

      response["hits"]["hits"].each do |activity|
        call_log.entries.create! level: "info", details: { activity: { "body" => activity["_source"] }.to_json }
      end
    end
  end
end
