class ChangeExternalServiceStepForeignKeyToGuid < ActiveRecord::Migration
  def up
    add_column :external_service_steps, :external_service_guid, :string
    add_index :external_service_steps, :external_service_guid

    connection.execute "UPDATE external_service_steps SET external_service_guid = (SELECT external_services.guid FROM external_services WHERE external_services.id = external_service_steps.external_service_id LIMIT 1)"

    remove_column :external_service_steps, :external_service_id
  end

  def down
    add_column :external_service_steps, :external_service_id, :integer
    add_index :external_service_steps, :external_service_id

    connection.execute "UPDATE external_service_steps SET external_service_id = (SELECT external_services.id FROM external_services WHERE external_services.guid = external_service_steps.external_service_guid LIMIT 1)"

    remove_column :external_service_steps, :external_service_guid
  end
end
