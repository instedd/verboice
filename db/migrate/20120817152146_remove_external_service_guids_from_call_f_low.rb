class RemoveExternalServiceGuidsFromCallFLow < ActiveRecord::Migration
  def up
    remove_column :call_flows, :external_service_guids
  end

  def down
    add_column :call_flows, :external_service_guids, :text
  end
end
