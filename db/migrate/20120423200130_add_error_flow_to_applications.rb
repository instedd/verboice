class AddErrorFlowToApplications < ActiveRecord::Migration
  def change
    add_column :applications, :error_flow, :binary
  end
end
