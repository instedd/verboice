class AddStatusCallbackUrlToApplication < ActiveRecord::Migration
  def change
    add_column :applications, :status_callback_url, :string
  end
end
