class PutsCommand < Command
  param :text, :string

  def initialize(str)
    @str = str
  end

  def run(context)
    puts @str
  end
end
