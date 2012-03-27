require 'spec_helper'

module Parsers
  module UserFlowNode
    describe Menu do

      it "should compile to a verboice equivalent flow" do
        menu = Menu.new id: 27, type: 'menu', data: {explanation_text: 'foo',
          options:[
            {
              description: 'foo',
              number: 1,
              next: 10
            },
            {
              description: 'bar',
              number: 2,
              next: 14
            }
          ]
        }
        menu_2 = Menu.new id: 10, type: 'menu', data: { explanation_text: 'foo', options:[] }
        menu_3 = Menu.new id: 14, type: 'menu', data: { explanation_text: 'foo', options:[] }
        menu.solve_links_with [ menu_2, menu_3 ]
    
        menu.equivalent_flow.should eq([
          {:capture => {:min => 1, :max => Float::INFINITY}},
          {:if => {:condition => 'timeout || finish_key', :then => [:hangup], :else => {:callback => {:url => 'http://www.domain.com/controller/action', :method => 'GET'}}}}
        ])
    
      end


      it "should be able to build itself from an incomming hash" do
        menu = Menu.new id: 27, type: 'menu', data: {explanation_text: 'foo'}
        menu.id.should eq(27)
        menu.explanation_text.should eq('foo')
      end
  
      it "should handle a menu input stream"do
        (Menu.can_handle? id: 27, type: 'menu', data: {}).should be_true
        (Menu.can_handle? id: 27, type: 'answer', data: {}).should be_false
      end
  
      it "should build with a collection of options" do
        menu = Menu.new id: 27, type: 'menu', data: {explanation_text: 'foo',
          options:[
            {
              description: 'foo',
              number: 1,
              next: 10
            },
            {
              description: 'bar',
              number: 2,
              next: 14
            }
          ]
        }
        menu.options.size.should eq(2)
        menu.options.first[:number].should eq(1)
        menu.options.first[:description].should eq('foo')
        menu.options.first[:next].should eq(10)
        menu.options.last[:number].should eq(2)
        menu.options.last[:description].should eq('bar')
        menu.options.first[:next].should eq(10)
      end

      it "should resolve it's next links from a given list of commands" do
        menu = Menu.new id: 27, type: 'menu', data: {explanation_text: 'foo',
          options:[
            {
              description: 'foo',
              number: 1,
              next: 10
            },
            {
              description: 'bar',
              number: 2,
              next: 14
            }
          ]
        }
        menu_2 = Menu.new id: 10, type: 'menu', data: { explanation_text: 'foo', options:[] }
        menu_3 = Menu.new id: 14, type: 'menu', data: { explanation_text: 'foo', options:[] }
    
        menu.solve_links_with [ menu_2, menu_3 ]
        menu.options[0][:next].should eq(menu_2)
        menu.options[1][:next].should eq(menu_3)
      end
  
      it "should respond if it's a root or not" do
        menu_1 = Menu.new id: 10, root: true, type: 'menu', data: { explanation_text: 'foo', options:[] }
        menu_2 = Menu.new id: 14, type: 'menu', data: { explanation_text: 'foo', options:[] }
        menu_1.is_root?.should be_true
        menu_2.is_root?.should be_false
      end
    end
  end
end