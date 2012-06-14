class AddGlobalSettingsToExternalService < ActiveRecord::Migration
  def change
    add_column :external_services, :global_settings, :text
  end
end
