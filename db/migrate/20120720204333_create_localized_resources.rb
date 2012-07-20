class CreateLocalizedResources < ActiveRecord::Migration
  def change
    create_table :localized_resources do |t|
      t.string :language
      t.string :text
      t.binary :audio
      t.string :url
      t.string :type
      t.references :resource

      t.timestamps
    end
    add_index :localized_resources, :resource_id
  end
end
