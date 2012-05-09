require 'spec_helper'

module Parsers
  module UserFlowNode
    describe Menu do

      let(:app) { Application.make }
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
          ],
          'default' => 45

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
        play4 = Play.new app, 'id' => 45,
            'type' => 'play',
            'name' => 'Play 45',
            'message' => {
              "name" => "Fifth explanation message",
              "type" => "text"
            }

        menu.solve_links_with [ play1, play2, play3, play4 ]

        menu.equivalent_flow.first.should eq(
          Compiler.parse do |c|
            c.Label 1
            c.Assign "current_step", 1
            c.PlayFile File.join(Rails.root, "data","applications","#{app.id}","recordings", "1-explanation.wav")
            c.Assign 'attempt_number1', '1'
            c.While 'attempt_number1 <= 3' do |c|
              c.Capture play_file: File.join(Rails.root, "data","applications","#{app.id}","recordings", "1-options.wav"), finish_on_key: '', timeout: 20
              c.Assign 'value_1', 'digits'
              c.PersistVariable 'some_variable', 'value_1'
              c.If "digits == '4'" do |c|
                c.Trace application_id: app.id, step_id: 1, step_name: 'Menu number one', store: '"User pressed: " + digits'
                c.Label 10
                c.Assign "current_step", 10
                c.Trace application_id: app.id, step_id: 10, step_name: 'Play 1', store: '"Message played."'
                c.Say "Second explanation message"
                c.Goto "end1"
              end
              c.If "digits == '6'" do |c|
                c.Trace application_id: app.id, step_id: 1, step_name: 'Menu number one', store: '"User pressed: " + digits'
                c.Label 14
                c.Assign "current_step", 14
                c.Trace application_id: app.id, step_id: 14, step_name: 'Play 2', store: '"Message played."'
                c.Say "Third explanation message"
                c.Goto "end1"
              end
              c.If "digits == '2'" do |c|
                c.Trace application_id: app.id, step_id: 1, step_name: 'Menu number one', store: '"User pressed: " + digits'
                c.Label 5
                c.Assign "current_step", 5
                c.Trace application_id: app.id, step_id: 5, step_name: 'Play 3', store: '"Message played."'
                c.Say "Fourth explanation message"
                c.Goto "end1"
              end
              c.If "digits != null" do |c|
                c.PlayFile File.join(Rails.root, "data","applications","#{app.id}","recordings", "1-invalid.wav")
                c.Trace application_id: app.id, step_id: 1, step_name: 'Menu number one', store: '"Invalid key pressed"'
              end
              c.Else do |c|
                c.Trace application_id: app.id, step_id: 1, step_name: 'Menu number one', store: '"No key was pressed. Timeout."'
              end
              c.Assign 'attempt_number1', 'attempt_number1 + 1'
            end
            c.Trace application_id: app.id, step_id: 1, step_name: 'Menu number one', store: '"Missed input for 3 times."'
            c.Label 45
            c.Assign "current_step", 45
            c.Trace application_id: app.id, step_id: 45, step_name: 'Play 45', store: '"Message played."'
            c.Say "Fifth explanation message"
            c.Label "end1"
          end.first
        )
      end

      it "should compile to a minimum verboice equivalent flow" do
        menu = Menu.new app, 'id' => 27, 'type' => 'menu'
        menu.equivalent_flow.make.should eq(
          Compiler.make do |c|
            c.Assign "current_step", 27
            c.Assign 'attempt_number27', '1'
            c.While 'attempt_number27 <= 3' do |c|
              c.Capture finish_on_key: '', timeout: 5
              c.Assign 'value_27', 'digits'
              c.If "digits != null" do |c|
                c.Trace application_id: app.id, step_id: 27, step_name: '', store: '"Invalid key pressed"'
              end
              c.Else do |c|
                c.Trace application_id: app.id, step_id: 27, step_name: '', store: '"No key was pressed. Timeout."'
              end
              c.Assign 'attempt_number27', 'attempt_number27 + 1'
            end
            c.Trace application_id: app.id, step_id: 27, step_name: '', store: '"Missed input for 3 times."'
            c.Label 'end27'
          end
        )
      end

      it "should be able to build itself from an incomming hash" do
        menu = Menu.new app, 'id' => 27, 'type' => 'menu', 'explanation_message' => {'name' => 'foo', 'type' => 'text'}, 'timeout' => 20, 'invalid_message' => {'name' => 'foobar', 'type' => 'text'}, 'options_message' => {}
        menu.id.should eq(27)
        menu.explanation_message.name.should eq('foo')
        menu.timeout.should eq(20)
        menu.invalid_message.name.should eq('foobar')
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
    end
  end
end