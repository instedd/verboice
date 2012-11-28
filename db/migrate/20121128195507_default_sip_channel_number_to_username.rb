class DefaultSipChannelNumberToUsername < ActiveRecord::Migration
  def up
    Channels::Sip.all.each do |channel|
      channel.config['number'] = channel.config['username']
      channel.save!
    end
  end

  def down
    Channels::Sip.all.each do |channel|
      channel.config.delete 'number'
      channel.save!
    end
  end
end
