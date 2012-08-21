class CallFlowExternalService < ActiveRecord::Base
  belongs_to :call_flow
  belongs_to :external_service

  validates_presence_of :call_flow, :external_service
  validates_uniqueness_of :call_flow_id, :scope => :external_service_id
end
