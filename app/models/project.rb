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

  belongs_to :account
  belongs_to :default_call_flow, :class_name => "CallFlow", :foreign_key => "call_flow_id"
  has_many :call_flows, :dependent => :destroy
  has_many :call_logs, :dependent => :destroy
  has_many :queued_calls, :dependent => :destroy
  has_many :external_services, :dependent => :destroy
  has_many :schedules, :dependent => :destroy

  attr_accessible :name, :account, :status_callback_url, :status_callback_url_user, :status_callback_url_password, :time_zone


  validates_presence_of :name
  validates_uniqueness_of :name, :scope => :account_id

  config_accessor :status_callback_url_user, :status_callback_url_password

  attr_encrypted :config, :key => ENCRYPTION_KEY, :marshal => true

  def call(address)
  end
end
