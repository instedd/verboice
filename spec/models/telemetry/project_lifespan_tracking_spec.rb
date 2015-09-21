require 'spec_helper'
include InsteddTelemetry

describe "project lifespan tracking" do

  before(:each) { Timecop.freeze(Time.now) }

  let!(:project) { Project.make }

  def verify_lifespan(timespan, project, last_update)
    timespan.since.to_i.should eq(project.created_at.to_i)
    timespan.until.to_i.should eq(last_update.to_i)
  end
  
  def should_update_lifespan(project, &block)
    Timecop.freeze(Time.now + 3.seconds)
    block.call
    verify_lifespan(Timespan.first, project, Time.now)
  end

  it "tracks project creation" do
    Timespan.count.should eq(1)
    verify_lifespan(Timespan.first, project, project.created_at)
  end

  it "tracks project settings update" do
    should_update_lifespan(project) do
      project.name = "new name"
      project.save
    end
  end

  [:call_flows, :schedules, :scheduled_calls, :external_services, :feeds].each do |relation_name|

    it "tracks creation, edition and deletion of #{relation_name}" do
      should_update_lifespan(project) do
        project.send(relation_name).make
      end
      
      child = project.send(relation_name).first

      should_update_lifespan(project) do
        child.name = "new name"
        child.save
      end

      should_update_lifespan(project) do
        child.destroy
      end
    end
    
  end

end
