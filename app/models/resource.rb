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

class Resource < ActiveRecord::Base

  belongs_to :project
  has_many :localized_resources, :dependent => :destroy

  accepts_nested_attributes_for :localized_resources

  attr_accessible :name, :localized_resources_attributes

  validates_presence_of :name, :project

  validates :guid, :presence => true, :uniqueness => { :scope => :project_id }

  broker_cached

  after_initialize do
    self.guid ||= Guid.new.to_s
  end

  def as_json(options = {})
    # Fix for rails not calling as_json on includes, remove when fixed
    include_localized_resources = false
    if options[:include] == :localized_resources
      options.delete :include
      include_localized_resources = true
    end
    result = super
    result[:localized_resources] = self.localized_resources.as_json if include_localized_resources
    result
  end

  def available_resource_for language
    resource = resource_for language
    resource = resource_for(project.default_language) unless resource.present?
    resource
  end

  def resource_for language
    localized_resources.detect do |localized_resource|
      localized_resource.language == language
    end
  end
end
