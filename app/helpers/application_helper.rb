module ApplicationHelper
  def time_ago(time)
    return '' if time.nil?
    '<span title="' << time.utc.to_s << '">' << time_ago_in_words(time.utc, true) << ' ago</span>'
  end

  def verbo_version
    begin
      @@verbo_version = File.read('VERSION').strip unless defined? @@verbo_version
    rescue Errno::ENOENT
      @@verbo_version = 'Development'
    end
    @@verbo_version
  end
end
