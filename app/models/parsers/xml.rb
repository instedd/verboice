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