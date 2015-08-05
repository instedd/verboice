class AddFailureDetailsToCallLogs < ActiveRecord::Migration
  def change
    add_column :call_logs, :fail_details, :string
    add_column :call_logs, :fail_code, :string
  end
end
