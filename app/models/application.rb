class Application < ActiveRecord::Base
  belongs_to :account
  has_many :call_logs, :dependent => :destroy

  before_validation :set_name_to_callback_url, :unless => :name?

  validates_presence_of :name
  validates_uniqueness_of :name, :scope => :account_id

  serialize :flow, Array

  def run(pbx, options = {})
    new_session(pbx, options).run
  end

  def new_session(pbx, options = {})
    session = Session.new
    session.pbx = pbx
    session.application = self
    session.call_log = options[:call_log] || call_logs.create!(:direction => :incoming)
    session.call_log.address = options[:caller_id]
    session.commands = self.commands.dup
    session
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
    call_log = call_logs.create! :direction => :outgoing
    call_log.info "Initiating call from API to #{address}"
    call_log.save!

    begin
      PbxClient.call address, self.id, call_log.id
    rescue Exception => ex
      call_log.error ex.message
      call_log.finish :failed
    end

    call_log
  end

  private

  def set_name_to_callback_url
    self.name = callback_url
  end

end
