class Application < ActiveRecord::Base
  serialize :flow, Array

  def run(pbx)
    session = Session.new
    session.pbx = pbx
    session.application = self
    session.commands = (self.flow || [:answer, {:callback => self.callback_url}]).dup

    session.run
  end
end
