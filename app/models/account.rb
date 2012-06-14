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

class Account < ActiveRecord::Base
  # Include default devise modules. Others available are:
  # :token_authenticatable, :confirmable, :lockable and :timeoutable
  devise :database_authenticatable, :registerable,
         :recoverable, :rememberable, :trackable, :validatable, :confirmable

  # Setup accessible (or protected) attributes for your model
  attr_accessible :email, :password, :password_confirmation, :remember_me

  has_many :projects, :dependent => :destroy
  has_many :call_flows, :through => :projects

  has_many :contacts, :through => :projects
  has_many :persisted_variables, :through => :contacts
  has_many :recorded_audios, :through => :contacts

  has_many :call_logs

  has_many :channels, :dependent => :destroy
  has_many :queued_calls, :through => :channels

  has_one :google_oauth_token, :class_name => 'OAuthToken', :conditions => {:service => :google}, :dependent => :destroy

  def call(options = {})
    channel = channels.find_by_name! options[:channel]
    channel.call options[:address], options
  end

end
