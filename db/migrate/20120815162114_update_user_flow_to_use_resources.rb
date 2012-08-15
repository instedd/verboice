class UpdateUserFlowToUseResources < ActiveRecord::Migration

  def up
    CallFlow.transaction do
      CallFlow.all.each do |call_flow|
        p "Processing call flow with id #{call_flow.id}"
        # user flow can be nil
        next if call_flow.user_flow.nil?
        call_flow.user_flow.each do |step|
          if self.respond_to? step['type'].to_sym
            self.send step['type'].to_sym, call_flow, step
          end
        end
        call_flow.save!
      end
    end
  end

  def down
  end

  # Steps

  def play(call_flow, step)
    process_message call_flow, step, 'message', 'resource', 'message'
  end

  def menu(call_flow, step)
    %w(invalid explanation options).each do |action|
      process_message call_flow, step, "#{action}_message", "#{action}_resource", action
    end
  end

  def capture(call_flow, step)
    %w(invalid instructions).each do |action|
      process_message call_flow, step, "#{action}_message", "#{action}_resource", action
    end
  end

  def record(call_flow, step)
    %w(explanation confirmation).each do |action|
      process_message call_flow, step, "#{action}_message", "#{action}_resource", action
    end
  end

  def language(call_flow, step)
    return if step['languages'].nil?

    resource = Resource.new name: "#{step['name'] || step['id']}"
    resource.project = call_flow.project
    resource.save

    step['resource'] = {'guid' => resource.guid}

    step['languages'].each do |language|
      next if language['message'].nil?
      action = LanguageList::LanguageInfo.find(language['key']).name
      update_localized_resource(call_flow, resource, step, language['message'], action, language['key'])
    end

    step.delete 'languages'
  end

  # Shared

  def process_message(call_flow, step, message_name, resource_name, action)
    # message can be nil
    return if step[message_name].nil?
    resource = update call_flow, step, step[message_name], action
    step.delete message_name
    step[resource_name] = {'guid' => resource.guid}
  end

  def update(call_flow, step, message, action)
    resource = Resource.new name: "#{step['name'] || step['id']} #{action}"
    resource.project = call_flow.project
    resource.save!

    update_localized_resource(call_flow, resource, step, message, action)

    resource
  end

  def update_localized_resource(call_flow, resource, step, message, action, language = nil)
    localized_resource = LocalizedResource.new
    localized_resource.resource = resource
    localized_resource.language = language || call_flow.project.default_language
    localized_resource.text = message['name']
    localized_resource.type = message['type'] == 'recording' ? 'RecordLocalizedResource' : 'TextLocalizedResource'
    if message['type'] == 'recording'
      localized_resource.duration = message['duration']
      localized_resource.recorded_audio = read_file call_flow.id, "#{step['id']}-#{action}.wav" rescue nil
    end
    localized_resource.save!
  end

  def read_file(call_flow_id, name)
    File.open File.join(Rails.root, "data", "call_flows", call_flow_id.to_s, "recordings", name) do |file|
      file.read
    end
  end

end
