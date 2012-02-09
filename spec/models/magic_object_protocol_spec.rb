require 'spec_helper'

describe MagicObjectProtocol do
  before(:each) do
    @server = EM.start_server 'localhost', 1234, ServerFoo
    @client = EM.connect 'localhost', 1234, MagicObjectProtocol::Client
  end

  after(:each) do
    @client.close_connection
    EM.stop_server @server
  end

  it "send object, then send an object raises" do
    # Send an object
    assert_equal 30, @client.add(10, 20)
    assert_equal 40, @client.add(20, 20)

    # Send an object raises
    begin
      result = @client.this_will_raise
      fail "Client expected to raise"
    rescue => ex
      ex.message.should == "This is the message"
    end
  end

  class ServerFoo < MagicObjectProtocol::Server
    def add(a, b)
      a + b
    end

    def this_will_raise
      raise "This is the message"
    end
  end
end

