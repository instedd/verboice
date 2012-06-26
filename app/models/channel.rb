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

class Channel < ActiveRecord::Base

  belongs_to :account
  belongs_to :call_flow
  has_one :project, :through => :call_flow

  has_many :call_logs, :dependent => :destroy
  has_many :queued_calls, :dependent => :destroy

  validates_presence_of :account
  validates_presence_of :call_flow

  validates_presence_of :name
  validates_uniqueness_of :name, :scope => :account_id

  after_commit :call_broker_create_channel, :if => :persisted?
  before_update :call_broker_delete_channel
  before_destroy :call_broker_delete_channel

  serialize :config, Hash

  def config
    self[:config] ||= {}
  end

  config_accessor :username
  config_accessor :password

  def self.inherited(child)
    # force all subclass to have :channel as model name
    child.instance_eval do
      def model_name
        Channel.model_name
      end
    end
    super
  end

  def new_session(options = {})
    session = Session.new options
    session.call_flow ||= call_flow
    session.channel = self
    unless session.call_log
      session.call_log = call_logs.new :direction => :incoming, :call_flow => session.call_flow, :account => account, :project => session.call_flow.project, :started_at => Time.now.utc
      session.call_log.start_incoming
    end
    session.commands = session.call_flow.commands.dup
    session
  end

  def call(address, options = {})

    queued_call = enqueue_call_to address, options
    call_log = queued_call.call_log

    begin
      if queued_call.not_before?
        broker_client.notify_call_queued id, queued_call.not_before
      else
        broker_client.notify_call_queued id
      end
    rescue Exception => ex
      call_log.finish_with_error ex.message
      queued_call.destroy
    end

    call_log
  end

  def enqueue_call_to address, options

    via = options.fetch(:via, 'API')

    current_call_flow = (CallFlow.find(options[:call_flow_id].presence) rescue nil) || call_flow
    flow = options[:flow] || current_call_flow.commands
    project_id = options[:project_id].presence || (CallFlow.find(options[:call_flow_id].presence) rescue nil).try(:project).try(:id) || call_flow.project.id

    project = Project.find(project_id)
    schedule = options.has_key?(:schedule_id) ? project.schedules.find(options[:schedule_id]) : nil
    schedule ||= options.has_key?(:schedule) ? project.schedules.find_by_name!(options[:schedule]) : nil

    time_zone = nil
    not_before = if options[:not_before].is_a?(String)
      time_zone = options[:time_zone].blank? ? ActiveSupport::TimeZone.new(current_call_flow.project.time_zone || 'UTC') : (ActiveSupport::TimeZone.new(options[:time_zone]) or raise "Time zone #{options[:time_zone]} not supported")
      time_zone.parse(options[:not_before]) unless options[:not_before].blank?
    else
      options[:not_before]
    end

    call_log = call_logs.new :direction => :outgoing, :call_flow_id => current_call_flow.id, :project_id => project_id, :address => address, :state => :queued, :schedule => schedule, :not_before => not_before
    call_log.info "Received via #{via}: call #{address}"
    call_log.save!

    queued_call = queued_calls.new(
      :call_log => call_log,
      :address => address,
      :callback_url => options[:callback_url],
      :status_callback_url => options[:status_callback_url],
      :flow => flow,
      :not_before => not_before,
      :schedule => schedule,
      :call_flow_id => current_call_flow.id,
      :project_id => project_id
    )

    queued_call.not_before = queued_call.schedule.with_time_zone(time_zone) do |time_zoned_schedule|
      time_zoned_schedule.next_available_time(queued_call.not_before || Time.now.utc)
    end if queued_call.schedule

    queued_call.save!

    queued_call
  end

  def active_calls_count
    broker_client.active_calls_count_for id
  end

  def poll_call
    self.class.transaction do
      queued_call = queued_calls.where('not_before IS NULL OR not_before <= ?', Time.now.utc).order(:created_at).first
      queued_call.destroy if queued_call
      queued_call
    end
  end

  def register?
    config['register'] == '1'
  end

  def has_limit?
    limit.present?
  end

  def limit
    subclass_responsibility
  end

  def broker_client
    @broker_client ||= BrokerClient.new port
  end

  def call_broker_create_channel
    broker_client.create_channel self.id
  end

  def call_broker_delete_channel
    broker_client.delete_channel self.id
  end

  def kind
    self.class.kind
  end

  def self.kind
    self.name.split('::').last
  end

  def self.kinds
    [["#{kind} channel", "#{name}-#{kind}"]]
  end

  def port
    subclass_responsibility
  end

  def self.can_handle? a_kind
    subclass_responsibility
  end

  def self.from_json(json)
    channel = (SuitableClassFinder.find_leaf_subclass_of self, suitable_for: (json[:kind])).new
    channel.name = json[:name]
    channel.username = json[:username]
    channel.password = json[:password]
    channel
  end
end