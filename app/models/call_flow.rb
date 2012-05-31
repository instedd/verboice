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
  attr_accessible :name, :error_flow, :flow, :user_flow, :callback_url

  belongs_to :project
  has_many :call_logs, :dependent => :destroy
  has_many :channels
  has_many :queued_calls, :dependent => :destroy
  has_many :traces, :dependent => :destroy
  has_one :account, :through => :project

  serialize :flow, Command
  serialize :error_flow, Command
  serialize :user_flow, SerializableArray

  before_validation :set_name_to_callback_url, :unless => :name?
  validates_presence_of :name
  validates_uniqueness_of :name, :scope => :project_id

  before_update :update_flow_with_user_flow
  before_save :clear_flow, :if => lambda { @mode == 'callback_url' }

  config_accessor :callback_url_user, :callback_url_password
  attr_encrypted :config, :key => ENCRYPTION_KEY, :marshal => true

  def commands
    self.flow.present? ? self.flow : Compiler.new.Answer().Callback(self.callback_url).make
  end

  def info
    self.flow.present? ? "custom flow" : "callback #{self.callback_url}"
  end

  def mode
    self.flow.present? ? :flow : :callback_url
  end

  def mode=(value)
    @mode = value.to_s
  end

  def step_names
    (Parsers::UserFlow.new self, user_flow).step_names
  end

  private

  def set_name_to_callback_url
    self.name = callback_url
  end

  def update_flow_with_user_flow
    if user_flow_changed?
      parser  = Parsers::UserFlow.new self, user_flow
      self.flow = parser.equivalent_flow
      self.error_flow = parser.error_flow
    end
    true
  end

  def clear_flow
    self.flow = nil
    true
  end

end
