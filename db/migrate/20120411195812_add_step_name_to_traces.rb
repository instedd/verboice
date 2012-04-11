class AddStepNameToTraces < ActiveRecord::Migration
  def change
    add_column :traces, :step_name, :string
  end
end
