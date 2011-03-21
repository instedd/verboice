class Application < ActiveRecord::Base
  belongs_to :account
  has_many :call_logs, :dependent => :destroy

  validates_presence_of :name
  validates_uniqueness_of :name, :scope => :account_id

  serialize :flow, Array

  def run(pbx, call_log = nil)
    session = Session.new
    session.pbx = pbx
    session.application = self
    session.log = call_log || create_call_log
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

  def call(address)
    call_log = create_call_log
    call_log.log 'I', "Initiating call from API to #{address}"
    call_log.save!

    client = EM.connect '127.0.0.1', 8787, MagicObjectProtocol::Client
    begin
      return client.call address, self.id, call_log.id
    ensure
      client.close_connection
    end
  end

  def create_call_log
    CallLog.create!(:account => account, :application => self, :state => :active, :details => '')
  end
end
