require 'spec_helper'

describe ExternalService do

  let(:external_service) { ExternalService.new }

  it 'updates the manifest' do
    external_service.url = 'service url'
    response = double('response', :to_str => 'new xml')
    RestClient.should_receive(:get).with('service url').and_return(response)

    external_service.update_manifest!

    external_service.reload.xml.should eq('new xml')
  end

  it 'has empty global setting upon creation' do
    external_service.global_settings.should be_empty
  end

end
