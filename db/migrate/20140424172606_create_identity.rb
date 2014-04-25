class CreateIdentity < ActiveRecord::Migration
  def change
    create_table :identities do |t|
      t.integer :account_id
      t.string :provider
      t.string :token

      t.timestamps
    end
  end
end
