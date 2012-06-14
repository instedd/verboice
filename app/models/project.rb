class Project < ActiveRecord::Base

  include FusionTablesPush

  class SerializableArray < Array
    include MarshalZipSerializable
  end

  belongs_to :account
  has_many :call_logs, :dependent => :destroy
  has_many :traces, :dependent => :destroy
  has_many :queued_calls, :dependent => :destroy

  before_validation :set_name_to_callback_url, :unless => :name?

  validates_presence_of :name
  validates_uniqueness_of :name, :scope => :account_id

  serialize :flow, Command
  serialize :error_flow, Command
  serialize :user_flow, SerializableArray

  before_update :update_flow_with_user_flow
  before_save :clear_flow, :if => lambda { @mode == 'callback_url' }

  config_accessor :callback_url_user, :callback_url_password,
                  :status_callback_url_user, :status_callback_url_password

  attr_encrypted :config, :key => ENCRYPTION_KEY, :marshal => true

  def clear_flow
    self.flow = nil
    true
  end

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

  def call(address)
  end

  def push_results(call_log)
    self.push_to_fusion_tables(call_log) #TODO: Delay it!
  end

  def update_flow_with_user_flow
    if user_flow_changed?
      parser  = Parsers::UserFlow.new self, user_flow
      self.flow = parser.equivalent_flow
      self.error_flow = parser.error_flow
    end
    true
  end

  def step_names
    (Parsers::UserFlow.new self, user_flow).step_names
  end

  private

  def set_name_to_callback_url
    self.name = callback_url
  end


end
