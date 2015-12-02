require 'spec_helper'
include InsteddTelemetry

describe "project lifespan tracking" do

  before(:each) { Timecop.freeze(Time.now) }

  let!(:account) { Account.make }
  let!(:project) { account.projects.make }

  it "tracks account and project creation" do
    Timespan.count.should eq(2)
    verify_lifespan(account, account.created_at)
    verify_lifespan(project, project.created_at)
  end

  it "tracks account settings update" do
    updates_lifespan([account]) do
      account.confirmed_at = Time.now
      account.save
    end
  end

  it "tracks project settings update" do
    updates_lifespan([project]) do
      project.name = "new name"
      project.save
    end
  end

  [:call_flows, :schedules, :scheduled_calls, :external_services, :feeds].each do |relation_name|

    it "tracks creation, edition and deletion of #{relation_name}" do
      updates_lifespan([account, project]) do
        create_child(project, relation_name)
      end
      
      child = project.send(relation_name).first

      updates_lifespan([account, project]) do
        child.name = "new name"
        child.save
      end

      updates_lifespan([account, project]) do
        child.destroy
      end
    end    
  end

  def account_timespan
    Timespan.find_by_bucket(:account_lifespan)
  end

  def project_timespan
    Timespan.find_by_bucket(:project_lifespan)
  end

  def get_timespan(record)
    return account_timespan if record.is_a? Account
    return project_timespan if record.is_a? Project
  end

  def verify_lifespan(record, last_update)
    timespan = get_timespan(record)
    timespan.since.to_i.should eq(record.created_at.to_i)
    timespan.until.to_i.should eq(last_update.to_i)
  end
  
  def updates_lifespan(records, &block)
    Timecop.freeze(Time.now + 3.seconds)
    block.call
    records.each { |r| verify_lifespan(r, Time.now) }
  end

  def create_child(project, relation_name)
    if relation_name == :scheduled_calls
      flow = project.call_flows.make
      channel = Channels::Twilio.make call_flow: flow
      project.scheduled_calls.make    call_flow: flow, channel: channel
    else
      project.send(relation_name).make
    end
  end

end
