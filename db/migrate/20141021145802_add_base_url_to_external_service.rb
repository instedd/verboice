class AddBaseUrlToExternalService < ActiveRecord::Migration
  def change
    add_column :external_services, :base_url, :string
  end
end
