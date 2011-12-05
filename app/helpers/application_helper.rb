module ApplicationHelper
  def short(msg, length = 15)
    return '' if msg.nil?
    msg.length > length ? (msg[0 ... length] + "...") : msg
  end

  def short_html(msg, length = 15)
    ('<span title="' << (h msg) << '">' << h(short(msg, length)) << '</span>').html_safe
  end

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

def section title, url, name, active_controllers = [name]
  active = active_controllers.any?{|controller| controller_name == controller.to_s }
  raw "<li class=\"#{active ? "active" : ""}\">#{link_to title, url}</li>"
end