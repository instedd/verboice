xml.instruct! :xml, version: "1.0"
xml.rss version: "2.0" do
  xml.channel do
    xml.title "Recorded Audios (#{feed.name})"
    xml.pubDate feed.created_at.to_s(:rfc822)
    xml.lastBuildDate (recorded_audios.first || feed).created_at.to_s(:rfc822)

    recorded_audios.each do |recorded_audio|
      xml.item do
        xml.title "#{recorded_audio.id}.wav"
        xml.description "CallId: #{recorded_audio.call_log_id}, Key: #{recorded_audio.key}, Description: #{recorded_audio.description}"
        xml.pubDate recorded_audio.created_at.to_s(:rfc822)
        xml.link recording_feed_url(feed.key, recorded_audio)
        xml.guid recording_feed_url(feed.key, recorded_audio)
      end
    end
  end
end
