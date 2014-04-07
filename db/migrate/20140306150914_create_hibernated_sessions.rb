class CreateHibernatedSessions < ActiveRecord::Migration
  def change
    create_table :hibernated_sessions do |t|
      t.string :session_id
      t.binary :data

      t.timestamps
    end
  end
end
