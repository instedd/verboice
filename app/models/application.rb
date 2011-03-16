class Application < ActiveRecord::Base
  belongs_to :account
  has_many :call_logs

  validates_presence_of :name
  validates_uniqueness_of :name, :scope => :account_id

  serialize :flow, Array

  def run(pbx)
    session = Session.new
    session.pbx = pbx
    session.application = self
    session.log = CallLog.create! :account => account, :application => self, :state => :active, :details => ''
    session.commands = self.commands.dup

    session.run
  end

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
end
