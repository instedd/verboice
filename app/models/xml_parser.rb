class XmlParser
  @parsers = []

  def self.parse(xml)
    @parsers.each do |parser|
      return parser.parse xml if parser.can_parse? xml
    end
  end

  def self.inherited(subclass)
    @parsers << subclass
  end
end
