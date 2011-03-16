class XmlParser
  @parsers = []
  def self.inherited(subclass)
    @parsers << subclass
  end

  def self.parse(xml)
    xml = Nokogiri.XML xml
    @parsers.each do |parser|
      if parser.can_parse? xml
        return parser.parse xml
      end
    end
  end
end

Dir["#{Rails.root}/app/models/parsers/*"].each do |file|
  ActiveSupport::Inflector.camelize(file[file.rindex('/') + 1 .. -4]).constantize
end
