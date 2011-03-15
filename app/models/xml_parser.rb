class XmlParser
  @parsers = []

  def self.parse(xml)
    xml = Nokogiri.XML xml
    @parsers.each do |parser|
      if parser.can_parse? xml
        return parser.parse xml
      end
    end
  end

  def self.inherited(subclass)
    @parsers << subclass
  end
end
