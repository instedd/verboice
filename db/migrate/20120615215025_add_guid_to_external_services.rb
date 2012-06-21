class AddGuidToExternalServices < ActiveRecord::Migration
  def change
    add_column :external_service_steps, :guid, :string
    add_index :external_service_steps, :guid
    add_column :external_services, :guid, :string
    add_index :external_services, :guid
    add_column :call_flows, :external_service_guids, :text
  end
end
