module ChannelSerialization
  extend ActiveSupport::Concern

  module InstanceMethods
  end

  module ClassMethods
    def from_json(json)
      channel = Channel.new
      channel.name = json[:name]
      channel.kind = json[:kind]
      channel.username = json[:username]
      channel.password = json[:password]
      channel
    end
  end
end
