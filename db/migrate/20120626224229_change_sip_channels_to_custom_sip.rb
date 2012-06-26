class ChangeSipChannelsToCustomSip < ActiveRecord::Migration
  def up
    connection.execute "UPDATE channels SET type = 'Channels::CustomSip' WHERE type = 'Channels::Sip'"
  end

  def down
    connection.execute "UPDATE channels SET type = 'Channels::Sip' WHERE type = 'Channels::CustomSip'"
  end
end
