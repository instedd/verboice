# Copyright (C) 2010-2012, InSTEDD
# 
# This file is part of Verboice.
# 
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

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

