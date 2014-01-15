class AddIndexToPbxLogsOnGuidAndId < ActiveRecord::Migration
  def change
    add_index :pbx_logs, [:guid, :id]
  end
end
