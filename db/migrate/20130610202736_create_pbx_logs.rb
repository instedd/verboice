class CreatePbxLogs < ActiveRecord::Migration
  def change
    create_table :pbx_logs do |t|
      t.string :guid
      t.text :details
      t.timestamps
    end
  end
end
