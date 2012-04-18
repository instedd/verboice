class Commands::DialCommand < Command
  attr_accessor :number
  attr_accessor :channel_name
  attr_accessor :caller_id

  def initialize(number, options = {})
    @number = number
    @channel_name = options[:channel]
    @caller_id = options[:caller_id]
  end

  def run(session)
    if @channel_name.present?
      channel = session.channel.account.channels.find_by_name @channel_name
    else
      channel = session.channel
    end

    address = BaseBroker.instance.get_dial_address channel, @number
    session.info "Dialing #{address}"

    @options = {}
    @options[:caller_id] = caller_id if caller_id
    session[:dial_status] = session.pbx.dial address, @options
    session.info "Dial completed with status '#{session[:dial_status]}'"

    super
  end
end