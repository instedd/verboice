class AddSessionVariablesToExternalServiceSteps < ActiveRecord::Migration
  def change
    add_column :external_service_steps, :session_variables, :text
  end
end
