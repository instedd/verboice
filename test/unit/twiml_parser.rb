require 'test_helper'

class TwimlParserTest < ActiveSupport::TestCase

  test "can parse" do
    xml = Nokogiri.XML('<Response><Say>Hello</Say></Response>')
    assert TwimlParser.can_parse?(xml)
  end

  test "cant parse" do
    xml = Nokogiri.XML('<phoneml></phoneml>')
    assert !TwimlParser.can_parse?(xml)
  end

  test "parse play" do
    assert_parse '<Response><Play>http://foo</Play></Response>', [:play => 'http://foo']
  end

  test "parse gather" do
    assert_parse '<Response><Gather/></Response>', [:gather => {}]
  end

  test "parse gather with attributes" do
    assert_parse '<Response><Gather timeout="3" finishOnKey="*" numDigits="4"/></Response>',
      [:gather => {:timeout => 3, :finish_on_key => "*", :min => 4, :max => 4}]
  end

  test "parse gather with emedded play" do
    assert_parse '<Response><Gather><Play>http://foo</Play></Gather></Response>',
      [:gather => {:play => 'http://foo'}]
  end

  def assert_parse(xml, result)
    xml = Nokogiri.XML(xml)
    assert_equal result, XmlParser.parse(xml)
  end
end
