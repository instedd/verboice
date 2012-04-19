class Commands::SayCommand < Command
  attr_accessor :text

  def initialize(text)
    @text = text
  end

  def run(session)
    session.info "Say '#{@text}'"
    session.pbx.say @text
    super
  end
end
