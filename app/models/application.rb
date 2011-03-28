class Application < ActiveRecord::Base
  belongs_to :account
  has_many :call_logs, :dependent => :destroy

  validates_presence_of :name
  validates_uniqueness_of :name, :scope => :account_id

  serialize :flow, Array

  def run(pbx, call_log = nil)
    new_session(pbx, call_log).run
  end

  def new_session(pbx, call_log = nil)
    session = Session.new
    session.pbx = pbx
    session.application = self
    session.log = call_log || create_call_log
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
    call_log = create_call_log
    call_log.info "Initiating call from API to #{address}"
    call_log.save!

    begin
      with_pbx_interface { |client| client.call address, self.id, call_log.id }
    rescue Exception => ex
      call_log.error ex.message
      call_log.finish :failed
    end

    call_log
  end

  def with_pbx_interface
    port = Rails.configuration.verboice_configuration[:pbx_interface_port].to_i
    client = EM.connect '127.0.0.1', port, MagicObjectProtocol::Client
    begin
      yield client
    ensure
      client.close_connection
    end
  end

  def create_call_log
    CallLog.create!(:account => account, :application => self, :state => :active, :details => '')
  end
end
