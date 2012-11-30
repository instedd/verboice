class DefaultSipChannelNumberToUsername < ActiveRecord::Migration
  def up
    Channels::Sip.all.each do |channel|
      channel.config['number'] = channel.config['username']

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
    Channels::Sip.all.each do |channel|
      channel.config.delete 'number'

      def channel.call_broker_create_channel
      end

      def channel.call_broker_update_channel
      end

      def channel.call_broker_destroy_channel
      end

      channel.save
    end
  end
end
