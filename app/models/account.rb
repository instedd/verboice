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
  DEVISE_MODULES = [:database_authenticatable, :registerable,
                    :recoverable, :rememberable, :trackable, :validatable, :omniauthable]
  DEVISE_CONFIRMABLE = Rails.configuration.verboice_configuration[:skip_account_confirmation] \
                        ? [] : [:confirmable]

  devise *(DEVISE_MODULES + DEVISE_CONFIRMABLE)

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
  has_many :nuntium_channels, :dependent => :destroy
  has_many :permissions, :dependent => :destroy
  has_many :alerts, :dependent => :destroy

  has_many :identities, dependent: :destroy

  has_one :google_oauth_token, :class_name => 'OAuthToken', :conditions => {:service => :google}, :dependent => :destroy
  has_many :enabled_channels, :class_name => 'Channel', :conditions => {:enabled => true}

  after_save :telemetry_track_activity

  def shared_projects
    ProjectPermission.where(account_id: id).includes(:project)
  end

  def shared_channels
    ChannelPermission.where(account_id: id).includes(:channel)
  end

  def find_project_by_id(project_id)
    project_id = project_id.to_i

    project = projects.find_by_id(project_id)
    return project if project

    shared_project = shared_projects.where(model_id: project_id).first
    return shared_project.project if shared_project

    nil
  end

  def readable_project_ids
    projects.pluck(:id) + ProjectPermission.where(account_id: id).pluck(:model_id)
  end

  def find_call_flow_by_id(flow_id)
    find_call_flow { CallFlow.find_by_id(flow_id.to_i) }
  end

  def find_call_flow_by_name(flow_name)
    find_call_flow { CallFlow.find_by_name(flow_name) }
  end

  def find_call_flow
    call_flow = yield
    project = call_flow.project

    if project.account_id == id
      return call_flow
    end

    if shared_projects.where(model_id: project.id).exists?
      return call_flow
    end

    nil
  end

  def find_channel_by_id(channel_id)
    channel = channels.find_by_id(channel_id)
    channel ||= shared_channels.find_by_model_id(channel_id).try(&:channel)
    channel
  end

  def find_channel_by_name(channel_name)
    channel = channels.find_by_name(channel_name)
    channel ||= shared_channels.all.map(&:channel).find { |c| c.name == channel_name }
    channel
  end

  def call(options = {})
    channel = if options[:channel].present?
      find_channel_by_name options[:channel]
    else
      find_channel_by_id options[:channel_id]
    end

    if channel
      options[:account] = self
      channel.call options[:address], options
    else
      raise "Channel not found: #{options[:channel] || options[:channel_id]}"
    end
  end

  def telemetry_track_activity
    InsteddTelemetry.timespan_since_creation_update(:account_lifespan, {account_id: self.id}, self)
  end
end
