class XmlParser
  @parsers = []
  def self.inherited(subclass)
    @parsers << subclass
  end

  def self.parse(xml)
    xml = Nokogiri.XML xml
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

Dir["#{Rails.root}/app/models/parsers/*"].each do |file|
  ActiveSupport::Inflector.camelize(file[file.rindex('/') + 1 .. -4]).constantize
end
