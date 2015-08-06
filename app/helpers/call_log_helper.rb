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

module CallLogHelper
  def call_log_fail_info(call_log, link=true)
    return nil if call_log.fail_reason.nil?
    info = content_tag(:span, "#{call_log.fail_reason.capitalize}. ")
    if call_log.fail_code
      fail_details = call_log.fail_details ? "#{call_log.fail_details} (#{call_log.fail_code})" : "Code #{call_log.fail_code}."
      info << link_to_if(link, fail_details, error_code_url(call_log.fail_code), class: 'call-log-fail-info')
    elsif call_log.fail_details
      info << content_tag(:span, "#{call_log.fail_details}.")
    end
    info
  end

  def error_code_url(code)
    url = "https://github.com/instedd/verboice/wiki/Error-codes"
    url << "##{code.gsub(':', '').downcase}" if code
    url
  end
end

