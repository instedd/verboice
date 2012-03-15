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

  def with_callback_url_fields(type = nil)
    type = type.to_s << "_" if type
    [nil, :_user, :_password].each do |field|
      yield("#{type}callback_url#{field}".to_sym)
    end
  end

end
