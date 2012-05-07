require 'spec_helper'

module Parsers
  module UserFlowNode
    describe Menu do

      let(:app) { self }
      it "should compile to a verboice equivalent flow" do
        menu = Menu.new app, 'id' => 1,
          'type' => 'menu',
          'name' => 'Menu number one',
          'store' => 'some_variable',
          'explanation_message' => {
            "name" => "Some explanation message",
            "type" => "recording",
            "file" => "file.wav",
            "duration" => 5
          },
          'options_message' => {
            "name" => "Some options message",
            "type" => "recording",
            "file" => "file.wav",
            "duration" => 5
          },
          'timeout'=> 20,
          'number_of_attempts' => 3,
          'invalid_message' => {
            "name" => "An invalid key was pressed",
            "type" => "recording",
            "file" => "file.wav",
            "duration" => 5
          },
          'end_call_message' => {
            "name" => "This call will end now",
            "type" => "recording",
            "file" => "file.wav",
            "duration" => 5
          },
          'options' => [
            {
              'description' => 'foo',
              'number' => 4,
              'next' => 10
            },
            {
              'description' => 'bar',
              'number' => 6,
              'next' => 14
            },
            {
              'description' => 'zzz',
              'number' => 2,
              'next' => 5
            }
          ]
        play1 = Play.new app, 'id' => 10,
          'type' => 'play',
          'name' => 'Play 1',
          'message' => {
            "name" => "Second explanation message",
            "type" => "text"
          }
        play2 = Play.new app, 'id' => 14,
          'type' => 'play',
          'name' => 'Play 2',
          'message' => {
            "name" => "Third explanation message",
            "type" => "text"
          }
        play3 = Play.new app, 'id' => 5,
          'type' => 'play',
          'name' => 'Play 3',
          'message' => {
            "name" => "Fourth explanation message",
            "type" => "text"
          }

        menu.solve_links_with [ play1, play2, play3 ]

        menu.equivalent_flow.first.should eq(
          Compiler.parse do
            Label 1
            Assign "current_step", 1
            PlayFile File.join(Rails.root, "data","applications","1","recordings", "1-explanation.wav")
            Assign 'attempt_number1', '1'
            While 'attempt_number1 <= 3' do
              Capture play_file: File.join(Rails.root, "data","applications","1","recordings", "1-options.wav"), finish_on_key: '', timeout: 20
              Assign 'value_1', 'digits'
              PersistVariable 'some_variable', 'value_1'
              If "digits == '4'" do
                Trace application_id: 1, step_id: 1, step_name: 'Menu number one', store: '"User pressed: " + digits'
                Label 10
                Assign "current_step", 10
                Trace application_id: 1, step_id: 10, step_name: 'Play 1', store: '"Message played."'
                Say "Second explanation message"
                Goto "end1"
              end
              If "digits == '6'" do
                Trace application_id: 1, step_id: 1, step_name: 'Menu number one', store: '"User pressed: " + digits'
                Label 14
                Assign "current_step", 14
                Trace application_id: 1, step_id: 14, step_name: 'Play 2', store: '"Message played."'
                Say "Third explanation message"
                Goto "end1"
              end
              If "digits == '2'" do
                Trace application_id: 1, step_id: 1, step_name: 'Menu number one', store: '"User pressed: " + digits'
                Label 5
                Assign "current_step", 5
                Trace application_id: 1, step_id: 5, step_name: 'Play 3', store: '"Message played."'
                Say "Fourth explanation message"
                Goto "end1"
              end
              If "digits != null" do
                PlayFile File.join(Rails.root, "data","applications","1","recordings", "1-invalid.wav")
                Trace application_id: 1, step_id: 1, step_name: 'Menu number one', store: '"Invalid key pressed"'
              end
              Else do
                Trace application_id: 1, step_id: 1, step_name: 'Menu number one', store: '"No key was pressed. Timeout."'
              end
              Assign 'attempt_number1', 'attempt_number1 + 1'
            end
            Trace application_id: 1, step_id: 1, step_name: 'Menu number one', store: '"Missed input for 3 times."'
            PlayFile File.join(Rails.root, "data","applications","1","recordings", "1-end_call.wav")
            End()
            Label "end1"
          end.first
        )
      end

      it "should compile to a minimum verboice equivalent flow" do
        menu = Menu.new app, 'id' => 27, 'type' => 'menu'
        menu.equivalent_flow.make.should eq(
          Compiler.make do
            Assign "current_step", 27
            Assign 'attempt_number27', '1'
            While 'attempt_number27 <= 3' do
              Capture finish_on_key: '', timeout: 5
              Assign 'value_27', 'digits'
              If "digits != null" do
                Trace application_id: 1, step_id: 27, step_name: '', store: '"Invalid key pressed"'
              end
              Else do
                Trace application_id: 1, step_id: 27, step_name: '', store: '"No key was pressed. Timeout."'
              end
              Assign 'attempt_number27', 'attempt_number27 + 1'
            end
            Trace application_id: 1, step_id: 27, step_name: '', store: '"Missed input for 3 times."'
            Label 'end27'
          end
        )
      end

      it "should be able to build itself from an incomming hash" do
        menu = Menu.new app, 'id' => 27, 'type' => 'menu', 'explanation_message' => {'name' => 'foo', 'type' => 'text'}, 'timeout' => 20, 'invalid_message' => {'name' => 'foobar', 'type' => 'text'} , 'end_call_message' => {'name' => 'cya', 'type' => 'text'}, 'options_message' => {}
        menu.id.should eq(27)
        menu.explanation_message.name.should eq('foo')
        menu.timeout.should eq(20)
        menu.invalid_message.name.should eq('foobar')
        menu.end_call_message.name.should eq('cya')
      end

      it "should handle a menu input stream"do
        (Menu.can_handle? 'id' => 27, 'type' => 'menu').should be_true
        (Menu.can_handle? 'id' => 27, 'type' => 'answer').should be_false
      end

      it "should build with a collection of options" do
        menu = Menu.new app, 'id' => 27, 'type' => 'menu', 'explanation_message' => {'name' => 'foo', 'type' => 'text'},
          'options' => [
            {
              'description' => 'foo',
              'number' => 1,
              'next' => 10
            },
            {
              'description' => 'bar',
              'number' => 2,
              'next' => 14
            }
          ]
        menu.options.size.should eq(2)
        menu.options.first['number'].should eq(1)
        menu.options.first['description'].should eq('foo')
        menu.options.first['next'].should eq(10)
        menu.options.last['number'].should eq(2)
        menu.options.last['description'].should eq('bar')
        menu.options.first['next'].should eq(10)
      end

      it "should resolve it's next links from a given list of commands" do
        menu = Menu.new app, 'id' => 27, 'type' => 'menu',
          'explanation_message' => {"name" => 'foo', 'type' => 'text'},
          'options' =>[
            {
              'description' => 'foo',
              'number' => 1,
              'next' => 10
            },
            {
              'description' =>'bar',
              'number' => 2,
              'next' => 14
            }
          ]
        menu_2 = Menu.new app, 'id' => 10,
          'type' => 'menu',
          'explanation_message' => {"name"=>'foo', 'type' => 'text'}
        menu_3 = Menu.new app, 'id' => 14,
          'type' => 'menu',
          'explanation_message' => {"name"=>'foo', 'type' => 'text'}

        menu.solve_links_with [ menu_2, menu_3 ]
        menu.options[0]['next'].should eq(menu_2)
        menu.options[1]['next'].should eq(menu_3)
      end

      it "should respond if it's a root or not" do
        menu_1 = Menu.new app, 'id' => 10,
          'root' => 1,
          'type' => 'menu',
          'explanation_message' => {"name"=>'foo', 'type' => 'text'}
        menu_2 = Menu.new app, 'id' => 14,
          'type' => 'menu',
          'explanation_message' => {"name"=>'foo', 'type' => 'text'}
        menu_1.is_root?.should be_true
        menu_2.is_root?.should be_false
      end

      def id
        1
      end
    end
  end
end