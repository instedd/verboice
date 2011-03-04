class PutsCommand
  def initialize(str)
    @str = str
  end

  def run(context)
    puts @str
  end
end
