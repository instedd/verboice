class Channel < ActiveRecord::Base
  include ChannelSerialization

  Kinds = %w(sip custom voxeo)

  attr_protected :guid

  belongs_to :account
  belongs_to :call_flow

  has_many :call_logs, :dependent => :destroy
  has_many :queued_calls

  validates_presence_of :account
  validates_presence_of :call_flow

  validates_presence_of :name
  validates_uniqueness_of :name, :scope => :account_id

  validates_numericality_of :limit, :only_integer => true, :greater_than => 0, :if => :has_limit?

  after_commit :call_broker_create_channel, :if => :persisted?
  before_update :call_broker_delete_channel
  before_destroy :call_broker_delete_channel
  before_create :create_guid

  serialize :config, Hash

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
    schedule = options.has_key?(:schedule_id) ? account.schedules.find(options[:schedule_id]) : nil
    schedule ||= options.has_key?(:schedule) ? account.schedules.find_by_name!(options[:schedule]) : nil

    via = options.fetch(:via, 'API')

  current_call_flow = (CallFlow.find(options[:call_flow_id].presence) rescue nil) || call_flow
    flow = options[:flow] || current_call_flow.flow
    project_id = options[:project_id].presence || (CallFlow.find(options[:call_flow_id].presence) rescue nil).try(:project).try(:id) || call_flow.project.id

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

  def config
    self[:config] ||= {}
  end

  config_accessor :username
  config_accessor :password
  config_accessor :limit

  config_accessor :host
  config_accessor :register
  config_accessor :direction

  config_accessor :dial_string

  config_accessor :token
  config_accessor :url

  def host_and_port?
    config['host_and_port'].present?
  end

  def host_and_port
    config['host_and_port'].split ':', 2
  end

  def register?
    config['register'] == '1'
  end

  def has_limit?
    config['limit'].present?
  end

  def servers
    hosts = config['host'] || []
    servers = []
    hosts.each_with_index do |host, i|
      servers << Server.new(host, config['register'][i], config['direction'][i])
    end
    servers.length == 0 ? [Server.new] : servers
  end

  def broker_client
    @broker_client ||= BrokerClient.new port
  end

  private

  def port
    key = case kind
    when 'voxeo' then :voxeo_broker_port
    else :local_pbx_broker_port
    end
    Rails.configuration.verboice_configuration[key].to_i
  end

  def call_broker_create_channel
    broker_client.create_channel self.id
  end

  def call_broker_delete_channel
    broker_client.delete_channel self.id
  end

  def create_guid
    self.guid ||= Guid.new.to_s
  end
end
