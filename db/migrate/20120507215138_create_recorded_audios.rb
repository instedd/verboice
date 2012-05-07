class CreateRecordedAudios < ActiveRecord::Migration
  def change
    create_table :recorded_audios do |t|
      t.references :contact
      t.references :call_log
      t.string :key
      t.string :description

      t.timestamps
    end
    add_index :recorded_audios, :contact_id
    add_index :recorded_audios, :call_log_id
  end
end
