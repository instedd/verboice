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
    hash.map do |k, v|
      k = "'#{k}'" if k =~ /\-/
      if v.respond_to? :to_hash
        "#{k}:{#{kov(v)}}"
      elsif k.to_s == 'valueUpdate' && v[0] != "'" && v[0] != '"'
        "#{k}:'#{v}'"
      else
        "#{k}:#{v}"
      end
    end.join(',')
  end

  def with_callback_url_fields(type = nil)
    type = type.to_s << "_" if type
    [nil, :_user, :_password].each do |field|
      yield("#{type}callback_url#{field}".to_sym, field == :_password ? :password_field : :text_field)
    end
  end

  def link_to_add_box(class_name, name, project, options={})
    new_object = class_name.to_s.camelize.constantize.new
    new_object.project = project
    fields = render "box", class_name => new_object, :expanded => true
    link_to_function(name, "add_box(this, \"#{escape_javascript(fields)}\")", options)
  end

  def link_to_add_fields(name, f, association, options={})
    link_to_add(name, f, association, 'add_fields', options)
  end

  def link_to_add(name, f, association, function_name, options={})
    new_object = f.object.class.reflect_on_association(association).klass.new
    fields = f.fields_for(association, new_object, :child_index => "new_#{association}") do |builder|
      render(association.to_s.singularize + "_fields", :f => builder)
    end
    link_to_function(name, "#{function_name}(this, \"#{association}\", \"#{escape_javascript(fields)}\")", options)
  end

  def link_to_remove_fields(name, form, options={})
    form.hidden_field(:_destroy) + link_to_function(name, "remove_fields(this)", options)
  end

  def project_owner?
    @project.account_id == current_account.id
  end

  def project_admin?
    @project_permission == "admin"
  end

  def channel_admin?
    @channel_permission == "admin"
  end

  def nuntium_configured?
    Pigeon.config.nuntium_configured?
  end

  def format_timestamp(time)
    "#{l(time.to_date, format: :long)}, #{time.strftime("%H:%M:%S")} (UTC)"
  end
end
