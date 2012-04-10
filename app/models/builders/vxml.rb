module Builders
  class Vxml
    
    def initialize
      init_xml_builder
    end
    
    def build
      xml = @xml_builder.to_xml
      init_xml_builder
      xml
      # <<-xml
      #   <?xml version="1.0" encoding="UTF-8"?><vxml version="2.1"><form><field name="capture" type="digits?length=2"><grammar type="text/gsl">[dtmf-1 dtmf-2 dtmf-3 dtmf-4 dtmf-5 dtmf-6 dtmf-7 dtmf-8 dtmf-9 dtmf-0]</grammar><prompt><break strength="2s"/>Guess the number, it's between 1 and 99</prompt><noinput><prompt>Try Again</prompt><reprompt/></noinput><nomatch><prompt>You pressed a wrong input. Try Again.</prompt><reprompt/></nomatch></field><block><submit next="http://staging.instedd.org:7000/" method="GET"/></block></form></vxml>
      # xml
    end
    
    def say text
      append_to_form do |xml|
        xml.block do
          xml.prompt text
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
          #   xml.submit_(:next => "http://staging.instedd.org:7000/?disconnect=true")
          #   xml.exit
          # end
          xml.form do
            @form = xml.parent
          end
          # dummy form
          # xml.form do
          #             xml.field(:name => "_dummyField") do
          #               xml.prompt
          #               xml.grammar "[(dummy grammar)]"
          #               xml.filled do
          #                 xml.prompt
          #               end
          #               xml.noinput do
          #                 xml.disconnect
          #               end
          #             end
          #           end
        end
      end
    end
    
  end
end