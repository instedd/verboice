class Application < ActiveRecord::Base
  belongs_to :account

  serialize :flow, Array

  def run(pbx)
    session = Session.new
    session.pbx = pbx
    session.application = self
    session.commands = self.commands.dup

    session.run
  end

  def commands
    self.flow || [:answer, {:callback => self.callback_url}]
  end
end
