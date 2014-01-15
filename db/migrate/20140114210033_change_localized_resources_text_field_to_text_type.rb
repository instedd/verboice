class ChangeLocalizedResourcesTextFieldToTextType < ActiveRecord::Migration
  def up
    change_column :localized_resources, :text, :text
  end

  def down
    change_column :localized_resources, :text, :string
  end
end
