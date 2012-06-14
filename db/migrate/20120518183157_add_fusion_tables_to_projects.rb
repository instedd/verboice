class AddFusionTablesToProjects < ActiveRecord::Migration
  def change
    add_column :projects, :fusion_table_name, :string
    add_column :projects, :current_fusion_table_id, :string
  end
end
