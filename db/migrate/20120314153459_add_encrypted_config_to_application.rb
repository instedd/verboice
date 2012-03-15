class AddEncryptedConfigToApplication < ActiveRecord::Migration
  def change
    add_column :applications, :encrypted_config, :text
  end
end
