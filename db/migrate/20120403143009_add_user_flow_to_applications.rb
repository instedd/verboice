class AddUserFlowToApplications < ActiveRecord::Migration
  def change
    add_column :applications, :user_flow, :text
  end
end
