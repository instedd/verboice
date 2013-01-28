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

module Builders
  class Twiml

    def initialize(callback_url, vars = {})
      @callback_url = callback_url
      @vars = vars
      init_xml_builder
    end

    def build
      xml = @xml_builder.to_xml
      init_xml_builder
      xml
    end

    def say(text)
      append_to_body do |xml|
        xml.Say text
      end

      self
    end

    def play(filename)
      append_to_body do |xml|
        xml.Play filename
      end

      self
    end

    def pause(time)
      append_to_body do |xml|
        xml.Pause length: time
      end

      self
    end

    def hangup
      append_to_body do |xml|
        xml.Hangup
      end

      self
    end

    def gather(options)
      max_digits = options[:max] || 1
      timeout = options[:timeout] || 10
      finish_on_key = options[:finish_on_key] || '#'

      append_to_body do |xml|
        xml.Gather(method: 'GET', timeout: timeout, finishOnKey: finish_on_key, numDigits: max_digits) do
          if options[:say]
            xml.Say options[:say]
          elsif options[:play]
            xml.Play options[:play]
          end
        end
      end

      append_to_body do |xml|
        xml.Redirect(method: 'GET') { xml.text('.') }
      end

      self
    end

    private

    def append_to_body
      builder = Nokogiri::XML::Builder.new do |xml|
        yield xml
      end
      @body.add_child builder.doc.root
    end

    def init_xml_builder
      @xml_builder = Nokogiri::XML::Builder.new do |xml|
        xml.Response do
          @body = xml.parent
        end
      end
    end
  end
end