class CreateFlowResultsDataPackages < ActiveRecord::Migration
  def change
    create_table :flow_results_data_packages do |t|
      t.integer     :id
      t.string      :uuid,             nullable: :false
      t.integer     :call_flow_id,     nullable: :false
      t.datetime    :from,             nullable: :false
      t.datetime    :until,            nullable: :false
      t.timestamps
    end
    add_index :flow_results_data_packages, :uuid
    add_index :flow_results_data_packages, :call_flow_id
  end
end
