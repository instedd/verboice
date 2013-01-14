class DeleteInvalidTextLocalizedResources < ActiveRecord::Migration
  def up
    CallFlow.all.each do |call_flow|
      puts "Processing call flow with id #{call_flow.id}"
      next if call_flow.user_flow.nil?
      call_flow.user_flow.each do |step|
        step.each do |key, value|
          if key.end_with?("resource") && value.is_a?(Hash)
            process_resource(step, value)
          end
        end
      end
      call_flow.save!
    end
  end

  def down
  end

  def process_resource(step, step_resource)
    guid = step_resource['guid']
    resource = Resource.includes(:localized_resources).find_by_guid(guid)
    if resource
      if resource.localized_resources.length == 1
        loc_res = resource.localized_resources[0]
        if loc_res.type == 'TextLocalizedResource' && loc_res.text.nil?
          resource.destroy
          step_resource.delete 'guid'
        end
      end
    else
      step_resource.delete 'guid'
    end
  end
end
