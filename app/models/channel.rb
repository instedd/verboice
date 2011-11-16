class Channel < ActiveRecord::Base
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

  def call(address)
    call_log = call_logs.new :direction => :outgoing, :application_id => application_id, :address => address, :state => :queued
    call_log.info "Received via API: call #{address}"
    call_log.save!

    queued_call = queued_calls.create! :call_log => call_log, :address => address

    begin
      BrokerClient.notify_call_queued id
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
      queued_call = queued_calls.order(:created_at).first
      queued_call.try :destroy
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
