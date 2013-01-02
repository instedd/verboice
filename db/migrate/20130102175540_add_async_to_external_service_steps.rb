class AddAsyncToExternalServiceSteps < ActiveRecord::Migration
  def change
    add_column :external_service_steps, :async, :boolean
  end
end
