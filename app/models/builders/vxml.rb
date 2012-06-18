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
  class Vxml

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
      append_to_dummy do |xml|
        xml.prompt text
      end

      self
    end

    def play(filename)
      append_to_dummy do |xml|
        xml.audio(:src => filename)
      end

      self
    end

    def pause(time)
      append_to_dummy do |xml|
        xml.prompt do
          xml.break(:time => "#{time}s")
        end
      end

      self
    end

    def hangup
      exit_with do |xml|
        xml.exit
      end

      self
    end

    def callback(url, method = :get)
      exit_with do |xml|
        xml.submit_(:next => url, :method => method.to_s.upcase, :namelist => @vars.keys.push('digits').join(' '))
      end

      self
    end

    def capture(options)
      min_digits = options[:min] || 1
      max_digits = options[:max] || 1
      timeout = options[:timeout] || 10

      exit_with do |xml|
        xml.goto(:next => '#capture_form')
      end

      append_to_capture_form do |xml|
        xml.field(:name => "digits", :type => "digits?minlength=#{min_digits};maxlength=#{max_digits}") do
          xml.property(:name => "timeout", :value => "#{timeout}s")
          xml.noinput do
            xml.assign(:name => 'digits', :expr => "''")
          end
          xml.nomatch do
            xml.assign(:name => 'digits', :expr => "''")
          end
          if options[:say]
            xml.prompt options[:say]
          elsif options[:play]
            xml.audio :src => options[:play]
          end
        end
      end

      self
    end

    private

    def append_to_dummy
      builder = Nokogiri::XML::Builder.new do |xml|
        yield xml
      end
      @dummy.add_child builder.doc.root
    end

    def append_to_capture_form
      builder = Nokogiri::XML::Builder.new do |xml|
        yield xml
      end
      @capture_form.add_child builder.doc.root
    end

    def exit_with
      builder = Nokogiri::XML::Builder.new do |xml|
        if @exited_from_dummy
          xml.block do
            yield xml
          end
        else
          xml.catch_ :event => 'noinput nomatch' do
            yield xml
          end
        end
      end
      if @exited_from_dummy
        @capture_form.add_child builder.doc.root
      else
        @dummy.add_child builder.doc.root
        @exited_from_dummy = true
      end
    end

    def init_xml_builder
      @exited_from_dummy = false
      @xml_builder = Nokogiri::XML::Builder.new do |xml|
        xml.vxml(:version => "2.1") do
          xml.catch_(:event => "connection.disconnect.hangup") do
            xml.var(:name => "disconnect", :expr => "true")
            xml.submit_(:next => @callback_url, :namelist => @vars.keys.concat(%w(disconnect)).join(' '))
            xml.exit
          end
          xml.error do
            xml.var(:name => "error", :expr => "true")
            xml.var(:name => "message", :expr => "_message")
            xml.var(:name => "event", :expr => "_event")
            xml.submit_(:next => @callback_url, :namelist => @vars.keys.concat(%w(error message event)).join(' '))
            xml.exit
          end
          @vars.each do |k, v|
            xml.var(:name => k, :expr => "'#{v}'")
          end
          xml.form do
            xml.field :name => 'dummy' do
              xml.property :name => 'timeout', :value => '0s'
              xml.grammar '[agrammarthatwillnevermatch]', :type => 'text/gsl'
              @dummy = xml.parent
            end
          end
          xml.form :id => 'capture_form' do
            @capture_form = xml.parent
          end
        end
      end
    end

  end
end