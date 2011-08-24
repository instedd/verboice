class Channel < ActiveRecord::Base
  Kinds = %w(sip2sip callcentric)

  belongs_to :account
  belongs_to :application

  has_many :call_logs, :dependent => :destroy

  validates_presence_of :account
  validates_presence_of :application

  validates_presence_of :name
  validates_uniqueness_of :name, :scope => :account_id
  
  validates_numericality_of :limit, :only_integer => true, :greater_than => 0, :if => Proc.new { |x| x.has_limit? }

  after_commit :call_pbx_create_channel, :if => :persisted?
  before_update :call_pbx_delete_channel
  before_destroy :call_pbx_delete_channel

  serialize :config, Hash

  delegate :new_session, :to => :application

  def new_session(pbx, options = {})
    application.new_session pbx, options.merge(:channel => self)
  end

  def call(address)
    call_log = call_logs.create! :direction => :outgoing, :application_id => application_id, :address => address
    call_log.info "Initiating call from API to #{address}"
    call_log.save!

    call = CallQueue.enqueue self, call_log, address

    begin
      PbxClient.try_call_from_queue self.id
    rescue Exception => ex
      call_log.error ex.message
      call_log.finish :failed
      call.destroy
    end

    call_log
  end
  
  def can_call?
    !has_limit? || call_logs.where('started_at IS NOT NULL AND finished_at IS NULL').count < limit.to_i
  end

  def config
    read_attribute(:config) || {}
  end

  def host_and_port?
    config['host_and_port'].present?
  end

  def host_and_port
    config['host_and_port'].split ':', 2
  end

  def user
    config['user']
  end

  def password
    config['password']
  end

  def register?
    config['register'] == '1'
  end
  
  def limit
    config['limit']
  end
  
  def has_limit?
    config['limit'].present?
  end

  private

  def call_pbx_create_channel
    PbxClient.create_channel self.id
  end

  def call_pbx_delete_channel
    PbxClient.delete_channel self.id
  end
end
