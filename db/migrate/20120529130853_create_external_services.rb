class CreateExternalServices < ActiveRecord::Migration
  def change
    create_table :external_services do |t|
      t.references :project
      t.string :name
      t.string :url
      t.text :xml

      t.timestamps
    end
    add_index :external_services, :project_id
  end
end
