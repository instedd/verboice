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
        script += gather(child)
      when 'Hangup'
        script << :hangup
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

    # TODO: actually twiml logic should be something like:
    # [
    #   {:capture => options},
    #   {:if => :timeout,
    #      :then => execute other actions in script...,
    #      :else => :callback
    #   }
    # ]
    #
    # But we don't have the if command, and we don't have the concept
    # of timeout yet, so for now just capture and callback.
    #
    # Also missing: use the action and verb of the Gather node.
    #
    # See: http://www.twilio.com/docs/api/twiml/gather
    [{:capture => options}, :callback]
  end
end
