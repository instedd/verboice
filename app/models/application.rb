class Application < ActiveRecord::Base
  serialize :flow, Array

  def run(context)
    Flow.new(context).run self.flow
  end
end
