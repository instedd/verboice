require 'spec_helper'

describe Voxeo::HttpContext do
  
  let :context do
    headers = "Header1: Header1Value\x00Header2: HeaderWith:Colon"
    query_string = "key1=value1&key2=value2"
    Voxeo::HttpContext.new headers, query_string
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
    context = Voxeo::HttpContext.new nil, nil
    context.headers.empty?.should be_true
    context.params.empty?.should be_true
  end
  
end