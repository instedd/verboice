class AddGuidToExistingResources < ActiveRecord::Migration
  def up
    Resource.find_each do |resource|
      resource.guid= Guid.new.to_s
      resource.save
    end
    LocalizedResource.find_each do |localized_resource|
      localized_resource.guid= Guid.new.to_s
      localized_resource.save
    end
  end
  def down
  end
end
