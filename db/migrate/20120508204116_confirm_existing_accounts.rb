class ConfirmExistingAccounts < ActiveRecord::Migration
  def up
    Account.update_all ["confirmed_at = ?", Time.now]
  end

  def down
  end
end
