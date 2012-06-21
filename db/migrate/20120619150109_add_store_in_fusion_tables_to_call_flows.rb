class AddStoreInFusionTablesToCallFlows < ActiveRecord::Migration
  def change
    add_column :call_flows, :store_in_fusion_tables, :boolean
  end
end
