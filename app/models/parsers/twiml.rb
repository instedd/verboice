# Copyright (C) 2010-2012, InSTEDD
#
# This file is part of Verboice.
#
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

class Parsers::Twiml < Parsers::Xml
  def self.can_parse?(xml)
    xml.root.name == 'Response'
  end

  def self.parse(xml)
    compiler = Compiler.new

    xml.root.children.each do |child|
      case child.name
      when 'Play'
        play(child, compiler)
      when 'Gather'
        gather(child, compiler)
      when 'Redirect'
        redirect(child, compiler)
      when 'Hangup'
        compiler.Hangup()
      when 'Say'
        say(child, compiler)
      when 'Pause'
        pause(child, compiler)
      when 'Bridge'
        bridge(child, compiler)
      when 'Record'
        record(child, compiler)
      when 'Dial'
        continue = dial(child, compiler)
        break unless continue
      else
        raise Exception.new "Invalid element '#{child.name}'"
      end
    end

    compiler.Label('end').make
  end

  private

  def self.play(xml, compiler)
    compiler.PlayUrl(xml.text.try(:strip))
  end

  def self.redirect(xml, compiler)
    compiler.Callback xml.text, :method => (xml.attributes['method'].try(:value) || 'post')
  end

  def self.say(xml, compiler)
    compiler.Say(xml.text.try(:strip), xml.attributes['language'].try(:value))
  end

  def self.pause(xml, compiler)
    compiler.Pause(xml.attributes['length'].try(:value).try(:to_i) || 1)
  end

  def self.gather(xml, compiler)
    options = {}
    options[:timeout] = xml.attributes['timeout'].value.to_i if xml.attributes['timeout']
    options[:finish_on_key] = xml.attributes['finishOnKey'].value if xml.attributes['finishOnKey']
    if xml.attributes['numDigits']
      options[:min] = options[:max] = xml.attributes['numDigits'].value.to_i
    else
      options[:min] = 1
      options[:max] = Float::INFINITY
    end

    callback_url = xml.attributes['action'].value if xml.attributes['action']
    callback_options = {:params => {:Digits => :digits}}
    callback_options[:method] = xml.attributes['method'].value if xml.attributes['method']

    xml.children.each do |child|
      case child.name
      when 'Play'
        options[:play] = child.text
      when 'Say'
        options[:say] = child.text
      end
    end

    # See: http://www.twilio.com/docs/api/twiml/gather
    #   1. Capture
    #   2. If timeout or finish_key => continue with next verbs
    #   3. Otherwise => callback
    compiler
      .Capture(options)
      .If('timeout || finish_key') { Goto "label#{xml.object_id}" }
      .Else {
        Callback(callback_url, callback_options)
        Goto 'end'
      }
      .Label("label#{xml.object_id}")
  end

  def self.bridge(xml, compiler)
    compiler.Bridge(xml.attributes['session_id'].try(:value))
  end

  def self.record(xml, compiler)
    options = {}
    options[:stop_keys] = xml.attributes['finishOnKey'].value if xml.attributes['finishOnKey']
    options[:timeout] = xml.attributes['timeout'].value if xml.attributes['timeout']
    key = Guid.new.to_s.force_encoding("UTF-8")  # force_encoding to avoid serializing as binary
    compiler.Record key, "Recorded from external TwilML flow", options

    if xml.attributes['action']
      callback_options = {}
      callback_options[:method] = xml.attributes['method'].value if xml.attributes['method']
      compiler.Callback xml.attributes['action'].value, callback_options
    end
  end

  def self.dial(xml, compiler)
    options = {}
    options[:channel] = xml.attributes['channel'].value if xml.attributes['channel']
    options[:caller_id] = xml.attributes['callerId'].value if xml.attributes['callerId']
    compiler.Dial(xml.text, options)
    continue = true

    if xml.attributes['action']
      continue = false
      callback_options = {:params => {:DialCallStatus => :dial_status}}
      callback_options[:method] = xml.attributes['method'].value if xml.attributes['method']
      compiler.Callback xml.attributes['action'].value, callback_options
    end

    continue
  end
end
