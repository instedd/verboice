class ContactStats
  attr_reader :contact
  attr_reader :first_call
  attr_reader :last_call
  attr_reader :last_call_flow_name
  attr_reader :last_used_channel
  attr_reader :last_successful_call
  attr_reader :last_successful_call_flow_name

  def initialize(contact, first_call, last_call, last_successful_call)
    @contact = contact
    
    @first_call = first_call.created_at if first_call
      
    if last_call
      @last_call = last_call.created_at
      @last_call_flow_name = last_call.call_flow_name
      @last_used_channel = last_call.channel_name
    end

    if last_successful_call
      @last_successful_call = last_successful_call.created_at
      @last_successful_call_flow_name = last_call.call_flow_name
    end
  end

  def self.for(project)
    contacts = \
      project.contacts
        .includes(:addresses)
        .includes(:recorded_audios)
        .includes(:persisted_variables)
        .includes(:project_variables)

    latest_calls_by_contact = \
      CallLog.joins(:call_flow)
              .joins("INNER JOIN contact_addresses ON contact_addresses.address = call_logs.address")
              .joins("INNER JOIN contacts ON contacts.id = contact_addresses.contact_id")
              .joins("INNER JOIN channels ON channels.id = call_logs.channel_id")
              .where(project_id: project.id)
              .select("contacts.id as contact_id, call_logs.created_at, call_flows.name as call_flow_name, channels.name as channel_name, call_logs.state as state")
              .order("contacts.id, call_logs.created_at DESC")
              .all

    compute_stats contacts, latest_calls_by_contact    
  end

  private  

  # Last calls is a dataset of call logs (joined with any other relevant data)
  # ordered by contact.id, then by call_log.created_at. This lets us compute all necessary
  # stats by running a single query to the DB, although the price we pay is to iterate
  # the whole call log history for the project in memory.
  # We strongly use the assumption of order(contact, created_at desc). 
  # We need to identify the oldest, latest and latest successful call for each contact.
  # For each contact, think of last_calls as a stream of all calls which goes backwards in time.
  def self.compute_stats(contacts, last_calls)
    i = 0

    contacts.order(:id).map do |contact| 
      if i >= last_calls.length || last_calls[i].contact_id > contact.id
        # If we exhausted the call stream or it's already on a newer contact, it means that contact has never called. We set all results to nil and continue to the next contact.
        first_call = nil
        last_call = nil
        last_successful = nil
      elsif contact.id == last_calls[i].contact_id
        # If the call stream is at the current contact, we're at that contact's latest call, so we save it.        
        last_call = last_calls[i]

        # Then, we loop through the stream while we are at the current contact. 
        while i < last_calls.length && last_calls[i].contact_id == contact.id
          # We're going back in time for the current contact, so each iteration yields an older call log.
          # We overwrite first_call_data taking that into account.
          first_call = last_calls[i]

          # Again, relying on order, we save the first successful (:completed) call we find for the current contact.
          last_successful_call ||= last_calls[i] if last_calls[i].state == :completed

          i = i + 1
        end 
      end
        
      ContactStats.new contact, first_call, last_call, last_successful_call
    end
  end
end