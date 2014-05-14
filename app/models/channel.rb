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

  has_many :call_logs, :dependent => :nullify
  has_many :queued_calls, :dependent => :destroy
  has_many :channel_permissions, :foreign_key => "model_id", :dependent => :destroy

  config_accessor :limit

  validates_presence_of :account

  validates_presence_of :name
  validates_uniqueness_of :name, :scope => :account_id
  validates_numericality_of :limit, :only_integer => true, :greater_than => 0, :if => :has_limit?

  after_commit :call_broker_create_channel, :on => :create
  after_commit :call_broker_update_channel, :on => :update
  after_commit :call_broker_destroy_channel, :on => :destroy

  serialize :config, Hash

  broker_cached

  def config
    self[:config] ||= {}
  end

  def self.inherited(child)
    # force all subclass to have :channel as model name
    child.instance_eval do
      def model_name
        Channel.model_name
      end
    end
    super
  end

  def call(address, options = {})
    queued_call = enqueue_call_to address, options
    call_log = queued_call.call_log

    begin
      if queued_call.not_before?
        BrokerClient.notify_call_queued id, queued_call.not_before
      else
        BrokerClient.notify_call_queued id
      end
    rescue Exception => ex
      call_log.warn "Unable to notify the broker about this new call. The call might be delayed"
    end

    call_log
  end

  def enqueue_call_to address, options
    via = options.fetch(:via, 'API')

    account = options[:account] || self.account

    if options[:call_flow_id]
      current_call_flow = account.find_call_flow_by_id(options[:call_flow_id])
    elsif options[:call_flow]
      current_call_flow = account.find_call_flow_by_name(options[:call_flow])
    elsif options[:flow]
      flow = options[:flow]
    elsif options[:callback_url]
      callback_url = options[:callback_url]
    else
      current_call_flow = call_flow
    end

    if current_call_flow
      project = current_call_flow.project
    elsif options[:project_id]
      project = account.find_project_by_id(options[:project_id])
    else
      project = self.project
    end

    schedule = options.has_key?(:schedule_id) ? project.schedules.find(options[:schedule_id]) : nil
    schedule ||= options.has_key?(:schedule) ? project.schedules.find_by_name!(options[:schedule]) : nil

    time_zone = nil
    not_before = nil
    if options[:not_before].nil? || options[:not_before].is_a?(String)
      time_zone = options[:time_zone].blank? ? ActiveSupport::TimeZone.new(project.time_zone || 'UTC') : (ActiveSupport::TimeZone.new(options[:time_zone]) or raise "Time zone #{options[:time_zone]} not supported")
      not_before = time_zone.parse(options[:not_before]) if options[:not_before].present?
    else
      not_before = options[:not_before]
    end

    session_id = options[:session_id]

    if session_id
      call_log = nil
    else
      call_log = call_logs.new(
        :account => account,
        :direction => :outgoing,
        :call_flow => current_call_flow,
        :project => project,
        :address => address,
        :state => :queued,
        :schedule => schedule,
        :not_before => not_before
      )
      call_log.save!
      call_log.info "Received via #{via}: call #{address}"
    end

    if options[:vars].is_a?(Hash)
      variables = {}
      options[:vars].each do |name, value|
        variables[name] = (value =~ /^\d+$/ ? value.to_i : value)
      end
    end

    callback_params = options[:callback_params] if options[:callback_params].is_a?(Hash)

    queued_call = queued_calls.new(
      :call_log => call_log,
      :address => address,
      :callback_url => callback_url,
      :status_callback_url => options[:status_callback_url],
      :flow => flow,
      :not_before => not_before,
      :schedule => schedule,
      :call_flow => current_call_flow,
      :project => project,
      :time_zone => (time_zone ? time_zone.tzinfo.identifier : nil),
      :variables => variables,
      :session_id => session_id,
      :callback_params => callback_params,
    )

    queued_call.not_before = queued_call.schedule.with_time_zone(time_zone) do |time_zoned_schedule|
      time_zoned_schedule.next_available_time(queued_call.not_before || Time.now.utc)
    end if queued_call.schedule

    queued_call.save!

    queued_call
  end

  def has_limit?
    limit.present?
  end

  def broker
    :asterisk_broker
  end

  def call_broker_create_channel
    BrokerClient.create_channel(id, broker) rescue nil
  end

  def call_broker_update_channel
    BrokerClient.create_channel(id, broker) rescue nil
  end

  def call_broker_destroy_channel
    BrokerClient.destroy_channel(id, broker) rescue nil
  end

  def active_calls
    BrokerClient.active_calls_by_channel(id)
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

  def errors_count
    0
  end

  def self.can_handle? a_kind
    subclass_responsibility
  end

  def self.from_json(json)
    channel = (SuitableClassFinder.find_leaf_subclass_of self, suitable_for: json[:kind]).new
    channel.from_json(json)
  end

  def from_json(json)
    self.name = json[:name]
    json_config = json[:config] || {}
    self.class.config_attrs.each do |attr|
      self.send("#{attr}=", json_config[attr])
    end
    self
  end

  def as_json(options = {})
    options = { only: [:name, :config] }.merge(options)
    super(options).merge({
      kind: kind.try(:downcase),
      call_flow: call_flow.try(:name)
    })
  end
end
