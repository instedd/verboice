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

class CallFlow < ActiveRecord::Base
  include FusionTablesPush

  attr_accessible :name, :error_flow, :flow, :user_flow, :callback_url, :mode, :callback_url_user, :callback_url_password, :external_service_guids, :store_in_fusion_tables, :fusion_table_name, :current_fusion_table_id

  belongs_to :project

  has_many :call_logs, :dependent => :destroy
  has_many :channels
  has_many :queued_calls, :dependent => :destroy
  has_many :traces, :dependent => :destroy

  has_one :account, :through => :project
  has_one :google_oauth_token, :through => :account

  serialize :flow,      Command
  serialize :user_flow, SerializableArray
  serialize :variables, Array
  serialize :external_service_guids, Array

  before_validation :set_name_to_callback_url, :unless => :name?

  validates_presence_of :name
  validates_uniqueness_of :name, :scope => :project_id

  validates_presence_of :fusion_table_name, :if => :store_in_fusion_tables
  validates_presence_of :google_oauth_token, :if => :store_in_fusion_tables

  before_save :clear_flow, :if => lambda { mode_callback_url?}
  before_save :clear_callback_url, :if => lambda { mode_flow? }
  before_save :update_flow_with_user_flow

  enum_attr :mode, %w(callback_url ^flow)
  config_accessor :callback_url_user, :callback_url_password
  attr_encrypted :config, :key => ENCRYPTION_KEY, :marshal => true

  def commands
    self.flow.present? ? self.flow : Compiler.new.Answer().Callback(self.callback_url).make
  end

  def info
    self.flow.present? ? "custom flow" : "callback #{self.callback_url}"
  end

  def step_names
    (Parsers::UserFlow.new self, user_flow).step_names
  end

  def error_flow
    Commands::TraceCommand.new call_flow_id: id, step_id: 'current_step', step_name: '', store: '"User hung up."'
  end

  def push_results(call_log)
    self.push_to_fusion_tables(call_log)
  end

  private

  def set_name_to_callback_url
    self.name = callback_url
  end

  def update_flow_with_user_flow
    if user_flow.presence && user_flow_changed?
      parser  = Parsers::UserFlow.new self, user_flow
      self.flow = parser.equivalent_flow
      self.variables = parser.variables.to_a
      self.external_service_guids = parser.external_service_guids.to_a
    end
    true
  end

  def clear_flow
    self.flow = self.user_flow = nil
    true
  end

  def clear_callback_url
    self.callback_url = self.callback_url_user = self.callback_url_password = nil
    true
  end

end
