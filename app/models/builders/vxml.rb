module Builders
  class Vxml
    
    def initialize
      init_xml_builder
    end
    
    def build
      xml = @xml_builder.to_xml
      init_xml_builder
      xml
    end
    
    def say(text)
      append_to_form do |xml|
        xml.block do
          xml.prompt text
        end
      end
      
      self
    end
    
    def play(filename)
      append_to_form do |xml|
        xml.block do
          xml.audio(:src => filename)
        end
      end
      
      self
    end
    
    def pause(time)
      append_to_form do |xml|
        xml.block do
          xml.prompt do
            xml.break(:time => "#{time}s")
          end
        end
      end
      
      self
    end
    
    def hangup
      append_to_form do |xml|
        xml.block do
          xml.exit
        end
      end
      
      self
    end
    
    def callback(url, method = :get)
      append_to_form do |xml|
        xml.block do
          xml.submit_(:next => url, :method => method.to_s.upcase)
        end
      end
      
      self
    end
    
    def capture(options)
      min_digits = options[:min] || 1
      max_digits = options[:max] || 1
      timeout = options[:timeout] || 10
      
      append_to_form do |xml|
        xml.field(:name => "digits", :type => "digits?minlength=#{min_digits};maxlength=#{max_digits}") do
          xml.grammar "[dtmf-1 dtmf-2 dtmf-3 dtmf-4 dtmf-5 dtmf-6 dtmf-7 dtmf-8 dtmf-9 dtmf-0]", :type => "text/gsl"
          xml.property(:name => "timeout", :value => "#{timeout}s")
          xml.noinput
          xml.nomatch
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
    
    def append_to_form
      builder = Nokogiri::XML::Builder.new do |xml|
        yield xml
      end
      @form.add_child builder.doc.root
    end
    
    def init_xml_builder
      @xml_builder = Nokogiri::XML::Builder.new do |xml|
        xml.vxml(:version => "2.1") do
          # xml.catch_(:event => "connection.disconnect.hangup") do
          #   xml.log(:expr => "'*** CATCH EVENT DISCONNECTED ***'")
          #   xml.submit_(:next => "http://staging.instedd.org:7000/?disconnect=true")
          #   xml.exit
          # end
          # xml.error do
          #   xml.log(:expr => "'*** CATCH ERROR ***'")
          #   xml.submit_(:next => "http://staging.instedd.org:7000/?disconnect=true")
          #   xml.exit
          # end
          xml.form do
            @form = xml.parent
            # xml.block do
            #   xml.throw_(:event => "error.foo")
            # end
            # xml.block do
            #   xml.prompt "Before disconnect"
            # end
            # xml.block do
            #   xml.disconnect
            # end
            # xml.field do
            #   # xml.property(:name => "timeout", :value => "1s")
            #   xml.grammar "[(kathy bates is really hot)]"
            #   xml.nomatch do
            #     xml.exit
            #   end
            #   xml.noinput do
            #     xml.exit
            #   end
            #   # xml.exit
            # end
          end
        end
      end
    end
    
  end
end