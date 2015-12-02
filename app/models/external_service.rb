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

class ExternalService < ActiveRecord::Base
  include Telemetry::ProjectTracking

  belongs_to :project
  has_many :external_service_steps, :autosave => true, :dependent => :destroy
  has_many :call_flow_external_services, :dependent => :destroy
  has_many :call_flows, :through => :call_flow_external_services

  attr_accessible :name, :url, :xml, :global_variables_attributes, :guid, :global_settings, :base_url

  validates :guid, :presence => true, :uniqueness => { :scope => :project_id }

  serialize :global_settings, Hash

  validates_presence_of :url

  broker_cached

  after_initialize do
    self.guid ||= Guid.new.to_s
  end

  before_create do
    # fill a base url with the url used to download the manifest
    self.url = "http://#{self.url}" unless self.url.match /^(http|https):\/\//
    uri = URI.parse(self.url)
    self.base_url = "#{uri.scheme}://#{uri.host}#{':' + uri.port.to_s if uri.port != 80}"
  end

  after_save do
    return unless base_url_changed?

    ExternalService.transaction do
      self.call_flows.each do |call_flow|
        call_flow.force_update_flow_with_user_flow!
        call_flow.save!
      end
    end
  end

  def update_manifest!
    response = RestClient.get self.url
    self.xml = response.to_str
    self.update_from_manifest!(xml)
  end

  def steps
    external_service_steps
  end

  def update_from_manifest(xml)
    Parsers::ExternalService.new(self).parse(xml).save
  end

  def update_from_manifest!(xml)
    Parsers::ExternalService.new(self).parse(xml).save
  end

  def self.create_from_manifest(xml)
    Parsers::ExternalService.new.parse(xml).tap do |service|
      service.save
    end
  end

  def global_variables_attributes=(attributes)
    attributes.each do |index, attrs|
      variable = global_variables.detect{|v| v.name == attrs[:name]}
      variable.value = attrs[:value] if variable
    end
  end

  def global_variable_value_for(name)
    global_variables.detect{|v| v.name == name}.try(:value)
  end

  def global_variables
    global_settings[:variables] ||= []
  end

  def global_variables=(vars)
    global_settings[:variables] = vars
  end

  # TODO this should be called in a before_destroy hook
  # however by doing this it appears call_flows is empty by that time
  def clean_call_flows
    ExternalService.transaction do
      self.call_flows.each do |call_flow|
        call_flow.clean_external_service self
        call_flow.save!
      end
    end
  end

  def to_absolute_url(url)
    # can't use URI.parse since url may be http://{domain}/path
    if url.match /(http|https):\/\//
      url
    else
      base = base_url
      base.chop! if base[-1] == '/'
      url = url[1..-1] if url[0] == '/'
      "#{base}/#{url}"
    end
  end

  class GlobalVariable
    attr_accessor :name, :display_name, :type, :value

    def initialize(opts = {})
      @name = opts[:name]
      @display_name = opts[:display_name]
      @type = opts[:type]
      @value = opts[:value]
    end

    def persisted?
      false
    end
  end

end
