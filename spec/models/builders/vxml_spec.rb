require 'spec_helper'

describe Builders::Vxml do
  
  let(:vxml) { Builders::Vxml.new }
  
  it "builds an empty vxml" do
    xml = <<-XML
      <?xml version="1.0" encoding="UTF-8"?>
      <vxml version="2.1"><form></form></vxml>
    XML
    
    vxml.build.should be_equivalent_to(xml)
  end
  
  it "builds a prompt block for says" do
    xml = <<-XML
      <?xml version="1.0" encoding="UTF-8"?>
      <vxml version="2.1">
        <form>
          <block>
            <prompt>Hello World</prompt>
          </block>
        </form>
      </vxml>
    XML
    
    vxml.say("Hello World").build.should be_equivalent_to(xml)
  end
  
end