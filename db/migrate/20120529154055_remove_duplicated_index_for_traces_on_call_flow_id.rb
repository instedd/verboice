class RemoveDuplicatedIndexForTracesOnCallFlowId < ActiveRecord::Migration
  def up
    remove_index :traces, :name => :index_traces_on_application_id if index_name_exists?(:traces, :index_traces_on_application_id, false)
  end
end