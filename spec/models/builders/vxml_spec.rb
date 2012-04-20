require 'spec_helper'

describe Builders::Vxml do
  
  let(:vxml) { Builders::Vxml.new }
  let(:vars) { Hash.new }
  
  it "builds an empty vxml" do
    vxml.build.should be_equivalent_to(build_xml)
  end
  
  it "builds a prompt block for says" do
    inner_xml = <<-XML
      <block>
        <prompt>Hello World</prompt>
      </block>
    XML
    
    vxml.say("Hello World").build.should be_equivalent_to(build_xml(inner_xml))
  end
  
  it "build a prompt block for pause" do
    inner_xml = <<-XML
      <block>
        <prompt>
          <break time="5s"/>
        </prompt>
      </block>
    XML
    
    vxml.pause(5).build.should be_equivalent_to(build_xml(inner_xml))
  end
  
  it "should hangup with exit within block" do
    inner_xml = <<-XML
      <block>
        <exit/>
      </block>
    XML
    
    vxml.hangup.build.should be_equivalent_to(build_xml(inner_xml))
  end
  
  it "should play using audio within block" do
    inner_xml = <<-XML
      <block>
        <audio src="foo.wav"/>
      </block>
    XML
    
    vxml.play("foo.wav").build.should be_equivalent_to(build_xml(inner_xml))
  end
  
  it "builds a callback using submit" do
    inner_xml = <<-XML
      <block>
        <submit next="http://www.foo.com/bar" method="GET" namelist="digits"/>
      </block>
    XML
    
    vxml.callback("http://www.foo.com/bar").build.should be_equivalent_to(build_xml(inner_xml))
  end
  
  it "builds a POST callback using submit" do
    inner_xml = <<-XML
      <block>
        <submit next="http://www.foo.com/bar" method="POST" namelist="digits"/>
      </block>
    XML
    
    vxml.callback("http://www.foo.com/bar", :post).build.should be_equivalent_to(build_xml(inner_xml))
  end
  
  it 'builds var for default variables' do
    vars.merge!({:key1 => 'value1', :key2 => 'value2'})
    outside_form = <<-XML
      <var name="key1" expr="'value1'"/>
      <var name="key2" expr="'value2'"/>
    XML
    
    Builders::Vxml.new(vars).build.should be_equivalent_to(build_xml(nil,outside_form))
  end
  
  context "capture" do
  
    it "should capture with say using field" do
      inner_xml = <<-XML
        <field name="digits" type="digits?minlength=1;maxlength=1">
          <property name="timeout" value="10s"/>
          <noinput><assign name="digits" expr="''"/></noinput>
          <nomatch><assign name="digits" expr="''"/></nomatch>
          <prompt>This is a capture</prompt>
        </field>
      XML
    
      vxml.capture({:say => "This is a capture"}).build.should be_equivalent_to(build_xml(inner_xml))
    end
    
    it "should capture with play using field" do
      inner_xml = <<-XML
        <field name="digits" type="digits?minlength=1;maxlength=1">
          <property name="timeout" value="10s"/>
          <noinput><assign name="digits" expr="''"/></noinput>
          <nomatch><assign name="digits" expr="''"/></nomatch>
          <audio src="capture.wav"/>
        </field>
      XML
    
      vxml.capture({:play => "capture.wav"}).build.should be_equivalent_to(build_xml(inner_xml))
    end
    
    it "should capture with options using field" do
      inner_xml = <<-XML
        <field name="digits" type="digits?minlength=5;maxlength=10">
          <property name="timeout" value="20s"/>
          <noinput><assign name="digits" expr="''"/></noinput>
          <nomatch><assign name="digits" expr="''"/></nomatch>
          <prompt>This is a capture</prompt
        </field>
      XML
    
      vxml.capture(:say => "This is a capture", :min => 5, :max => 10, :timeout => 20).build.should be_equivalent_to(build_xml(inner_xml))
    end
  end
  
  def build_xml(inner_xml = '', outside_form = '')
    vars_list = vars.keys.concat(%w(error message event)).join(' ')
    <<-XML
      <?xml version="1.0" encoding="UTF-8"?>
      <vxml version="2.1">
        <error>
          <var name="error" expr="true"/>
          <var name="message" expr="_message"/>
          <var name="event" expr="_event"/>
          <submit next="http://staging.instedd.org:7000/" namelist="#{vars_list}"/>
          <exit/>
        </error>
        #{outside_form}
        <form>
          #{inner_xml}
        </form>
      </vxml>
    XML
  end
  
end