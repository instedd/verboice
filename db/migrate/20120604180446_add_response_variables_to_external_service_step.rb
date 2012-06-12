class AddResponseVariablesToExternalServiceStep < ActiveRecord::Migration
  def change
    add_column :external_service_steps, :response_variables, :text
  end
end
