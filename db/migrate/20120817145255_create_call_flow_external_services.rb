class CreateCallFlowExternalServices < ActiveRecord::Migration
  def change
    create_table :call_flow_external_services do |t|
      t.references :call_flow
      t.references :external_service

      t.timestamps
    end
    add_index :call_flow_external_services, :call_flow_id
    add_index :call_flow_external_services, :external_service_id
  end
end
