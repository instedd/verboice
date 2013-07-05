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

class Channels::TemplateBasedSip < Channels::Sip
  config_accessor :kind

  class << self
    attr_reader :templates
  end

  def domain
    self.class.templates[kind]
  end

  def direction
    'both'
  end

  def self.kinds
    templates.inject [] do |kinds, template|
      kinds << ["#{template[0]} channel", "#{name}-#{template[0]}"]
      kinds
    end
  end

  def register?
    true
  end

  def self.can_handle? a_kind
    templates.keys.any? do |template_name|
      template_name.downcase == a_kind
    end
  end

  @templates = YAML::load_file("#{Rails.root}/config/sip_channel_templates.yml").with_indifferent_access

  templates.each do |template_name, domain|
    define_singleton_method "new_#{template_name.underscore}_channel" do
      template = Channels::TemplateBasedSip.new
      template.kind = template_name
      template
    end
  end
end
