class CreateOAuthTokens < ActiveRecord::Migration
  def change
    create_table :o_auth_tokens do |t|
      t.integer :account_id
      t.string :service
      t.string :access_token
      t.string :refresh_token
      t.datetime :expires_at

      t.timestamps
    end
  end
end
