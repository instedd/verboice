class AddVariablesToCallFlows < ActiveRecord::Migration
  def change
    add_column :call_flows, :variables, :text
  end
end
