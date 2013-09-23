class CreateFeeds < ActiveRecord::Migration
  def change
    create_table :feeds do |t|
      t.string :name
      t.string :key
      t.references :project

      t.timestamps
    end
    add_index :feeds, :key
    add_index :feeds, :project_id
  end
end
