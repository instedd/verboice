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

class Project < ActiveRecord::Base

  serialize :languages

  belongs_to :account
  belongs_to :default_call_flow, :class_name => "CallFlow", :foreign_key => "call_flow_id"

  has_many :call_flows, :dependent => :destroy
  has_many :call_logs, :dependent => :nullify
  has_many :queued_calls, :dependent => :destroy
  has_many :external_services, :dependent => :destroy
  has_many :external_service_steps, :through => :external_services
  has_many :schedules, :dependent => :destroy
  has_many :contacts, :dependent => :destroy
  has_many :persisted_variables, :through => :contacts
  has_many :project_variables, :dependent => :destroy, :inverse_of => :project
  has_many :resources, :dependent => :destroy
  has_many :localized_resources, through: :resources

  accepts_nested_attributes_for :project_variables,
    :reject_if => lambda { |attributes| attributes[:name].blank?},
    :allow_destroy => true

  attr_accessible :name, :account, :status_callback_url, :status_callback_url_user, :status_callback_url_password, :time_zone, :project_variables_attributes, :languages, :default_language

  validates_presence_of :name
  validates_uniqueness_of :name, :scope => :account_id

  config_accessor :status_callback_url_user, :status_callback_url_password

  attr_encrypted :config, :key => ENCRYPTION_KEY, :marshal => true

  after_initialize :init

  before_validation :sanitize_languages

  def call(address)
  end

  def defined_variables
    project_variables.collect(&:name)
  end

  def update_variables_with variable_names
    variable_names.each do |variable_name|
      unless project_variables.any? {|var| var.name == variable_name}
        project_variables.create! name: variable_name
      end
    end
  end

  private

  def init
    self.default_language ||= 'en'
    self.languages ||= [self.default_language]
  end

  def sanitize_languages
    self.languages = Set.new(languages.reject{|l| l.blank?}).to_a
  end

end
