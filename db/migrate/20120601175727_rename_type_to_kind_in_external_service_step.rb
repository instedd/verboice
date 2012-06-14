class RenameTypeToKindInExternalServiceStep < ActiveRecord::Migration

  def change
    rename_column :external_service_steps, :type, :kind
  end

end
