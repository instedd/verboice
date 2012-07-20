class CreateResources < ActiveRecord::Migration
  def change
    create_table :resources do |t|
      t.string :name
      t.references :project

      t.timestamps
    end
    add_index :resources, :project_id
  end
end
