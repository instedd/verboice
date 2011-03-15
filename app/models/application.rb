class Application < ActiveRecord::Base
  serialize :flow, Array

  def run(context)
    f = Flow.new(context)
    if self.flow
      f.run self.flow
    else
      context.callback_url = self.callback_url
      f.run [:answer, {:callback => self.callback_url}]
    end
  end
end
