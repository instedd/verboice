class UpdateCallFlowExternalServices < ActiveRecord::Migration

  class CallFlowExternalService < ActiveRecord::Base
    belongs_to :call_flow
    belongs_to :external_service
  end

  class CallFlow < ActiveRecord::Base
    serialize :external_service_guids, Array
    has_many :call_flow_external_services
  end

  class ExternalService < ActiveRecord::Base
  end

  def up
    CallFlow.all.each do |call_flow|
      external_services = ExternalService.where('guid in (?)', call_flow.external_service_guids)
      external_services.each do |external_service|
        call_flow.call_flow_external_services.create! external_service_id: external_service.id
      end
    end
  end

  def down
    CallFlowExternalService.all.each do |call_flow_external_service|
      call_flow = call_flow_external_service.call_flow
      call_flow.external_service_guids << call_flow_external_service.external_service.guid
      call_flow.save!
    end
    CallFlowExternalService.destroy_all
  end
end
