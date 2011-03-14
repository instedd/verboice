class TwimlParser < XmlParser
  def self.can_parse?(xml)
    xml.root.name == 'Response'
  end

  def self.parse(xml)
    script = []
    xml.root.children.each do |child|
      case child.name
      when 'Play'
        script << play(child)
      when 'Gather'
        script << gather(child)
      end
    end
    script
  end

  private

  def self.play(xml)
    {:play => xml.text}
  end

  def self.gather(xml)
    options = {}
    options[:timeout] = xml.attributes['timeout'].value.to_i if xml.attributes['timeout']
    options[:finish_on_key] = xml.attributes['finishOnKey'].value if xml.attributes['finishOnKey']
    options[:min] = options[:max] = xml.attributes['numDigits'].value.to_i if xml.attributes['numDigits']

    xml.children.each do |child|
      case child.name
      when 'Play'
        options[:play] = child.text
      end
    end

    {:capture => options}
  end
end
