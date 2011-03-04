class AsteriskAdapter
  def initialize(context)
    @context = context
  end

  def answer
    @context.answer
  end
end
