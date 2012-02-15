class Channel < ActiveRecord::Base
  include ChannelSerialization

  Kinds = %w(generic sip2sip callcentric custom)

  belongs_to :account
  belongs_to :application

  has_many :call_logs, :dependent => :destroy
  has_many :queued_calls

  validates_presence_of :account
  validates_presence_of :application

  validates_presence_of :name
  validates_uniqueness_of :name, :scope => :account_id

  validates_numericality_of :limit, :only_integer => true, :greater_than => 0, :if => :has_limit?

  after_commit :call_broker_create_channel, :if => :persisted?
  before_update :call_broker_delete_channel
  before_destroy :call_broker_delete_channel

  serialize :config, Hash

  def new_session(options = {})
    session = Session.new options
    session.application ||= application
    session.channel = self
    unless session.call_log
      session.call_log = call_logs.new :direction => :incoming, :application => application, :started_at => Time.now.utc
      session.call_log.start_incoming
    end
    session.commands = session.application.commands.dup
    session
  end

  def call(address, options = {})
    call_log = call_logs.new :direction => :outgoing, :application_id => application_id, :address => address, :state => :queued
    call_log.info "Received via API: call #{address}"
    call_log.save!

    queued_call = queued_calls.new(
      :call_log => call_log,
      :address => address,
      :callback_url => options[:callback_url],
      :status_callback_url => options[:status_callback_url],
      :flow => options[:flow],
      :not_before => options[:not_before],
      :call_queue => options.has_key?(:queue) ? account.call_queues.find_by_name!(options[:queue]) : nil
    )

    if queued_call.call_queue_id? && queued_call.call_queue.time_from && queued_call.call_queue.time_to
      not_before = queued_call.not_before || Time.now
      queued_time = not_before - not_before.at_beginning_of_day
      queue_from = queued_call.call_queue.time_from - queued_call.call_queue.time_from.at_beginning_of_day
      queue_to = queued_call.call_queue.time_to - queued_call.call_queue.time_to.at_beginning_of_day

      if queued_time < queue_from
        queued_call.not_before = not_before + queue_from - queued_time
      elsif queued_time > queue_to
        queued_call.not_before = (not_before + 1.day).at_beginning_of_day + queue_from
      end
    end

    queued_call.save!

    begin
      if queued_call.not_before?
        BrokerClient.notify_call_queued id, queued_call.not_before
      else
        BrokerClient.notify_call_queued id
      end
    rescue Exception => ex
      call_log.finish_with_error ex.message
      queued_call.destroy
    end

    call_log
  end

  def active_calls_count
    BrokerClient.active_calls_count_for id
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

  private

  def call_broker_create_channel
    BrokerClient.create_channel self.id
  end

  def call_broker_delete_channel
    BrokerClient.delete_channel self.id
  end
end
