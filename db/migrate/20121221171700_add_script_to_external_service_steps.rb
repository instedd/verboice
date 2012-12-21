class AddScriptToExternalServiceSteps < ActiveRecord::Migration
  def change
    add_column :external_service_steps, :script, :text
  end
end
