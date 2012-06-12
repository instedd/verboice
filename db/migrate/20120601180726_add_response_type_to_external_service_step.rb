class AddResponseTypeToExternalServiceStep < ActiveRecord::Migration
  def change
    add_column :external_service_steps, :response_type, :string
  end
end
