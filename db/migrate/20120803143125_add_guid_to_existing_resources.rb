class AddGuidToExistingResources < ActiveRecord::Migration
  def up
    Resource.find_each do |resource|
      connection.execute "UPDATE resources SET guid = '#{Guid.new.to_s}' WHERE id = #{resource.id}"
    end
    LocalizedResource.find_each do |localized_resource|
      connection.execute "UPDATE localized_resources SET guid = '#{Guid.new.to_s}' WHERE id = #{localized_resource.id}"
    end
  end
  def down
  end
end
