class Parsers::Xml
  @parsers = []
  def self.inherited(subclass)
    @parsers << subclass
  end

  def self.parse(xml)
    xml = Nokogiri.XML(xml) { |config| config.options = Nokogiri::XML::ParseOptions::DEFAULT_XML | Nokogiri::XML::ParseOptions::NOBLANKS }
    raise_xml_parse_error xml.errors.first.message if xml.errors.any?

    @parsers.each do |parser|
      if parser.can_parse? xml
        begin
          return parser.parse xml
        rescue Exception => ex
          raise_xml_parse_error ex.message
        end
      end
    end

    raise_xml_parse_error 'unknown format'
  end

  private

  def self.raise_xml_parse_error(reason)
    raise Exception.new "Failed to parse the XML: #{reason}"
  end
end