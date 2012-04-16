class ChangeTraceStepIdFromIntegerToString < ActiveRecord::Migration
  def up
    remove_column :traces, :step_id
    add_column :traces, :step_id, :string
  end

  def down
    remove_column :traces, :step_id
    add_column :traces, :step_id, :integer
  end
end
