class ChangeResourceForeignKeyToId < ActiveRecord::Migration
  def up
    add_column :localized_resources, :resource_id, :integer
    add_index :localized_resources, :resource_id

    connection.execute "UPDATE localized_resources SET resource_id = (SELECT resources.id FROM resources WHERE resources.guid = localized_resources.resource_guid LIMIT 1)"

    remove_column :localized_resources, :resource_guid
  end

  def down
    add_column :localized_resources, :resource_guid, :string
    add_index :localized_resources, :resource_guid

    connection.execute "UPDATE localized_resources SET resource_guid = (SELECT resources.guid FROM resources WHERE resources.id = localized_resources.resource_id LIMIT 1)"

    remove_column :localized_resources, :resource_id
  end
end
