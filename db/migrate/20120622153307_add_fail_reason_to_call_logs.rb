class AddFailReasonToCallLogs < ActiveRecord::Migration
  def change
    add_column :call_logs, :fail_reason, :string
  end
end
