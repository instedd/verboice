require 'spec_helper'

describe CallFlowExternalService do

  before(:all) do
    CallFlowExternalService.make
  end

  it { should validate_presence_of(:call_flow) }
  it { should validate_presence_of(:external_service) }
  it { should validate_uniqueness_of(:call_flow_id).scoped_to(:external_service_id) }

end
