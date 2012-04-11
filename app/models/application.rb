class Application < ActiveRecord::Base
  belongs_to :account
  has_many :call_logs, :dependent => :destroy
  has_many :traces, :dependent => :destroy

  before_validation :set_name_to_callback_url, :unless => :name?

  validates_presence_of :name
  validates_uniqueness_of :name, :scope => :account_id

  serialize :flow, Array
  serialize :user_flow, Array

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
    self.flow.present? ? self.flow : [:answer, {:callback => self.callback_url}]
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

  def update_flow_with_user_flow
    if user_flow_changed?
      self.flow = (Parsers::UserFlow.new self, user_flow).equivalent_flow
    end
    true
  end

  private

  def set_name_to_callback_url
    self.name = callback_url
  end


end
