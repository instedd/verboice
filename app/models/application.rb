class Application < ActiveRecord::Base
  belongs_to :account
  has_many :call_logs, :dependent => :destroy

  before_validation :set_name_to_callback_url, :unless => :name?

  validates_presence_of :name
  validates_uniqueness_of :name, :scope => :account_id

  serialize :flow, Array

  def commands
    self.flow || [:answer, {:callback => self.callback_url}]
  end

  def info
    self.flow ? "custom flow" : "callback #{self.callback_url}"
  end

  def mode
    self.flow ? :flow : :callback_url
  end

  def mode=(value)
    # This is just for the UI
  end

  def call(address)
  end

  private

  def set_name_to_callback_url
    self.name = callback_url
  end

end
