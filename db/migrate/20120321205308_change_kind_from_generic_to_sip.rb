class ChangeKindFromGenericToSip < ActiveRecord::Migration
  def up
    Channel.where(:kind => 'generic').update_all(:kind => 'sip')
  end

  def down
    Channel.where(:kind => 'sip').update_all(:kind => 'generic')
  end
end
