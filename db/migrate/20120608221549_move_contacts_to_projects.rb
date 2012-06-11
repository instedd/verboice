class MoveContactsToProjects < ActiveRecord::Migration

  def up
    add_column :contacts, :project_id, :integer
    connection.execute "UPDATE contacts SET project_id = (SELECT projects.id FROM projects WHERE projects.account_id = contacts.account_id LIMIT 1)"
    remove_column :contacts, :account_id
    add_index :contacts, ["project_id"], :name => "index_contacts_on_project_id"
  end

  def down
    remove_index :contacts, :name => "index_contacts_on_project_id"
    add_column :contacts, :account_id, :integer
    connection.execute "UPDATE contacts SET account_id = (SELECT projects.account_id FROM projects WHERE projects.id = contacts.project_id LIMIT 1)"
    remove_column :contacts, :project_id
  end

end
