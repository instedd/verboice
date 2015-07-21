class AddIndicesToVariables < ActiveRecord::Migration
  def change
    add_index :persisted_variables, [:contact_id, :project_variable_id, :value], name: 'index_vars_on_contact_id_and_var_id_and_value'
    add_index :persisted_variables, [:contact_id, :implicit_key, :value], name: 'index_vars_on_contact_id_and_key_and_value'
  end
end
