class TwimlParser < XmlParser
  def self.can_parse?(xml)
    xml.root.name == 'Response'
  end

  def self.parse(xml)
    script = []
    main_script = script

    xml.root.children.each do |child|
      case child.name
      when 'Play'
        script << play(child)
      when 'Gather'
        commands, timeout_or_finish_key_commands = gather(child)
        commands.each { |cmd| script << cmd }

        script = timeout_or_finish_key_commands
      when 'Redirect'
        script << redirect(child)
      when 'Hangup'
        script << :hangup
      when 'Say'
        script << say(child)
      when 'Pause'
        script << pause(child)
      when 'Bridge'
        script << bridge(child)
      when 'Dial'
        commands, continue = dial(child)
        script.concat commands
        break unless continue
      else
        raise Exception.new "Invalid element '#{child.name}'"
      end
    end
    main_script
  end

  private

  def self.play(xml)
    {:play_url => xml.text}
  end

  def self.redirect(xml)
    {:callback => {:url => xml.text, :method => (xml.attributes['method'].try(:value) || 'post')}}
  end

  def self.say(xml)
    {:say => xml.text}
  end

  def self.pause(xml)
    {:pause => xml.attributes['length'].try(:value).try(:to_i) || 1}
  end

  def self.gather(xml)
    options = {}
    options[:timeout] = xml.attributes['timeout'].value.to_i if xml.attributes['timeout']
    options[:finish_on_key] = xml.attributes['finishOnKey'].value if xml.attributes['finishOnKey']
    if xml.attributes['numDigits']
      options[:min] = options[:max] = xml.attributes['numDigits'].value.to_i
    else
      options[:min] = 1
      options[:max] = Float::INFINITY
    end

    callback_options = {:params => {:Digits => :digits}}
    callback_options[:url] = xml.attributes['action'].value if xml.attributes['action']
    callback_options[:method] = xml.attributes['method'].value if xml.attributes['method']

    xml.children.each do |child|
      case child.name
      when 'Play'
        options[:play] = child.text
      when 'Say'
        options[:say] = child.text
      end
    end

    timeout_or_finish_key_commands = []

    # See: http://www.twilio.com/docs/api/twiml/gather
    #   1. Capture
    #   2. If timeout or finish_key => continue with next verbs
    #   3. Otherwise => callback
    all_commands = [
      {:capture => options},
      {:if => {:condition => 'timeout || finish_key',
               :then => timeout_or_finish_key_commands,
               :else => {:callback => callback_options}}
      }
    ]

    [all_commands, timeout_or_finish_key_commands]
  end

  def self.bridge(xml)
    {:bridge => xml.attributes['session_id'].try(:value)}
  end

  def self.dial(xml)
    options = { :number => xml.text }
    options[:channel] = xml.attributes['channel'].value if xml.attributes['channel']
    options[:caller_id] = xml.attributes['callerId'].value if xml.attributes['callerId']
    commands = [{:dial => options}]
    continue = true

    if xml.attributes['action']
      continue = false
      callback_options = {:url => xml.attributes['action'].value, :params => {:DialCallStatus => :dial_status}}
      callback_options[:method] = xml.attributes['method'].value if xml.attributes['method']
      commands << {:callback => callback_options}
    end

    [commands, continue]
  end
end
