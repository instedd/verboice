class CreateExternalServiceSteps < ActiveRecord::Migration
  def change
    create_table :external_service_steps do |t|
      t.integer :external_service_id
      t.string :name
      t.string :display_name
      t.string :icon
      t.string :type
      t.string :callback_url
      t.text :variables

      t.timestamps
    end

    add_index :external_service_steps, :external_service_id
  end
end
