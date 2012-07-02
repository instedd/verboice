class CreateProjectVariables < ActiveRecord::Migration
  def change
    create_table :project_variables do |t|
      t.references :project
      t.string :name

      t.timestamps
    end
    add_index :project_variables, :project_id
  end
end