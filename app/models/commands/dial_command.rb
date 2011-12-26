class DialCommand < Command
  param :number, :string, :ui_length => 80
  param :channel, :string, :ui_length => 20

  def initialize(options = {})
    @number = options[:number]
    @channel_name = options[:channel]
  end

  def run(session)
    address = BaseBroker.instance.get_dial_address session.channel, @number
    session.log :info => "Dialing #{address}"
    session.pbx.dial address
  end
end