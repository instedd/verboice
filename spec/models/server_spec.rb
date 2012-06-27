require 'spec_helper'

describe Server do
  it "register with '1'" do
    Server.new('host', '1').should be_register
  end

  it "not register with '0'" do
    Server.new('host', '0').should_not be_register
  end

  it "register with true" do
    Server.new('host', true).should be_register
  end

  it "not register with false" do
    Server.new('host', false).should_not be_register
  end
end