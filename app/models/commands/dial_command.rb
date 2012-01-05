class DialCommand < Command
  param :number, :string, :ui_length => 80
  param :channel, :string, :ui_length => 20

  def initialize(options = {})
    @number = options[:number]
    @channel_name = options[:channel]
  end

  def run(session)
    if @channel_name.present?
      channel = session.channel.account.channels.find_by_name @channel_name
    else
      channel = session.channel
    end

    address = BaseBroker.instance.get_dial_address channel, @number
    session.log :info => "Dialing #{address}"
    session[:dial_status] = session.pbx.dial address
  end
end