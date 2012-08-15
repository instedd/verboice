class AddGuidToResourcesAndLocalizedResources < ActiveRecord::Migration
  def change
    add_column :localized_resources, :guid, :string
    add_index :localized_resources, :guid
    add_column :resources, :guid, :string
    add_index :resources, :guid
    add_column :call_flows, :resource_guids, :text
  end
end
