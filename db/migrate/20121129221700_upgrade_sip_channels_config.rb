class UpgradeSipChannelsConfig < ActiveRecord::Migration
  def up
    Channels::Sip.all.each do |channel|

      host = channel.config['host']
      host = host.first if host.is_a?(Array)
      channel.config.delete 'host'
      channel.config['domain'] = host

      register = channel.config['register']
      register = register.first if register.is_a?(Array)
      channel.config['register'] = register

      direction = channel.config['direction']
      direction = direction.first if direction.is_a?(Array)
      channel.config['direction'] = direction

      def channel.call_broker_create_channel
      end

      def channel.call_broker_update_channel
      end

      def channel.call_broker_destroy_channel
      end

      channel.save
    end
  end

  def down
  end
end
