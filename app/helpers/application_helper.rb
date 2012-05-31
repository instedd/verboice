# Copyright (C) 2010-2012, InSTEDD
# 
# This file is part of Verboice.
# 
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

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
  
  def ko(hash = {})
    {'data-bind' => kov(hash)}
  end

  def kov(hash = {})
    hash.map{|k, v| "#{k}:#{v}"}.join(',')
  end

  def with_callback_url_fields(type = nil)
    type = type.to_s << "_" if type
    [nil, :_user, :_password].each do |field|
      yield("#{type}callback_url#{field}".to_sym, field == :_password ? :password_field : :text_field)
    end
  end

end
