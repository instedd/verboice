class AddStatusCallbackIncludeVarsToProjects < ActiveRecord::Migration
  def change
    add_column :projects, :status_callback_include_vars, :boolean, default: false
  end
end
