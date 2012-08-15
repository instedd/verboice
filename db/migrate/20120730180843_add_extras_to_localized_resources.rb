class AddExtrasToLocalizedResources < ActiveRecord::Migration
  def change
    add_column :localized_resources, :extras, :text
  end
end
