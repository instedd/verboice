class AddImplicitKeyToPersistedVariables < ActiveRecord::Migration
  def change
    add_column :persisted_variables, :implicit_key, :string
  end
end
