require 'spec_helper'

describe ChannelsController do
  let(:account) { Account.make }
  let(:project) { account.projects.make }
  let!(:call_flow) { CallFlow.make project: project }

  let(:other_project) { Project.make }

  before(:each) do
    sign_in account
  end

  it "shows call flows in new form" do
    get :new, type: "Channels::Custom", template: "Custom"
    expect(assigns(:channel)).to be_a_new(Channel)
    expect(assigns(:projects)).to eq([project])
  end

  it "load shared projects in new form" do
    Permission.create!(account_id: account.id, type: "Project", model_id: other_project.id, role: :admin)
    get :new, type: "Channels::Custom", template: "Custom"
    expect(assigns(:channel)).to be_a_new(Channel)
    expect(assigns(:projects)).to eq([project, other_project])
  end
end
