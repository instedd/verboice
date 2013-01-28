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

describe HttpBroker::HttpContext do

  let :context do
    headers = "Header1: Header1Value\x00Header2: HeaderWith:Colon"
    query_string = "key1=value1&key2=value2"
    HttpBroker::HttpContext.new headers, query_string
  end

  it "should tell headers" do
    context.headers[:Header1].should eq("Header1Value")
    context.headers[:Header2].should eq("HeaderWith:Colon")
  end

  it "should tell params" do
    context.params[:key1].should eq('value1')
    context.params[:key2].should eq('value2')
  end

  it "should have empty headers and params for nil headers and qs" do
    context = HttpBroker::HttpContext.new nil, nil
    context.headers.empty?.should be_true
    context.params.empty?.should be_true
  end

  it "should unescape query string" do
    value = "a value that & needs to be escaped"
    context = HttpBroker::HttpContext.new nil, "key=#{CGI.escape(value)}"
    context.params[:key].should eq(value)
  end

  it "should support empty query string values" do
    qs = "key="
    context = HttpBroker::HttpContext.new nil, qs
    context.params[:key].should eq('')
  end

end