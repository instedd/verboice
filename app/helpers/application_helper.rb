module ApplicationHelper
  def time_ago(time)
    return '' if time.nil?
    '<span title="' << time.utc.to_s << '">' << time_ago_in_words(time.utc, true) << ' ago</span>'
  end

  def verboice_version
    begin
      @@verboice_version = File.read('VERSION').strip unless defined? @@verbo_version
    rescue Errno::ENOENT
      @@verboice_version = 'Development'
    end
    @@verboice_version
  end
end
