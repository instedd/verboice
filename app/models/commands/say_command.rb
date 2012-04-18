class Commands::SayCommand < Command
  param :text, :string, :ui_length => 80

  def initialize(text)
    @text = text
  end

  def run(session)
    session.info "Say '#{@text}'"
    session.pbx.say @text
  end
end
