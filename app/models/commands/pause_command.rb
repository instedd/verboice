class Commands::PauseCommand < Command
  attr_accessor :length

  def initialize(length = 1)
    @length = length
  end

  def run(session)
    session.pbx.pause @length
  end
end
