module ExternalServiceHelper

  def destroy_message_for(external_service)
    call_flows = external_service.call_flows.pluck(:name)
    if call_flows.size > 0
      "This external service is being used in the following call flows: #{call_flows.join(', ')}. "\
      "Deleting will remove all these steps from each of those call flows. "\
      "Are you sure you want to proceed?"
    else
      "Are you sure?"
    end
  end

end
