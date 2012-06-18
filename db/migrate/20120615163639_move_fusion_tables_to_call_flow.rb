class MoveFusionTablesToCallFlow < ActiveRecord::Migration
  def change
    remove_column :projects, :fusion_table_name
    remove_column :projects, :current_fusion_table_id

    add_column :call_flows, :fusion_table_name, :string
    add_column :call_flows, :current_fusion_table_id, :string
  end
end
