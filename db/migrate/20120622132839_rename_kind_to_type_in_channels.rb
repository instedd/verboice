class RenameKindToTypeInChannels < ActiveRecord::Migration
  def up
    rename_column :channels, :kind, :type

   connection.execute "UPDATE channels SET type = 'Channels::Voxeo' WHERE type = 'voxeo'"
   connection.execute "UPDATE channels SET type = 'Channels::Custom' WHERE type = 'custom'"
   connection.execute "UPDATE channels SET type = 'Channels::Sip' WHERE type = 'sip'"
  end

  def down
    rename_column :channels, :type, :kind

    connection.execute "UPDATE channels SET kind = 'voxeo' WHERE kind = 'Channels::Voxeo'"
    connection.execute "UPDATE channels SET kind = 'custom' WHERE kind = 'Channels::Custom'"
    connection.execute "UPDATE channels SET kind = 'sip' WHERE kind = 'Channels::Sip'"
  end
end