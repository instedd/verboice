require 'spec_helper'

module Parsers
  module UserFlowNode
    describe Capture do

      let(:app) { Application.make }

      it "should compile to a verboice equivalent flow" do
        capture = Capture.new app,
          'id' => 1,
          'root' => true,
          'type' => 'capture',
          'name' => 'Capture number one',
          'store' => 'some_variable',
          'instructions_message' => { "name" => 'First Capture', 'type' => 'text' },
          'invalid_message' => {
            "name" => "An invalid key was pressed",
            "type" => "recording",
            "file" => "file.wav",
            "duration" => 5
          },
          'valid_values' => '1,2-4,10-20',
          'finish_on_key' => '#',
          'min_input_length' => 1,
          'max_input_length' => 2,
          'timeout' => 10

        capture.equivalent_flow.first.should eq(
          Compiler.parse do |c|
            c.Label 1
            c.Assign "current_step", 1
            c.Assign 'attempt_number1', '1'
            c.While 'attempt_number1 <= 3' do |c|
              c.Capture say: "First Capture", min: 1, max: 2, finish_on_key: '#', timeout: 10
              c.Assign 'value_1', 'digits'
              c.PersistVariable 'some_variable', 'value_1'
              c.If "(digits == 1) || (digits >= 2 && digits <= 4) || (digits >= 10 && digits <= 20)" do |c|
                c.Trace application_id: app.id, step_id: 1, step_name: 'Capture number one', store: '"User pressed: " + digits'
                c.Goto "end1"
              end
              c.If "digits != null" do |c|
                c.PlayFile File.join(Rails.root, "data","applications","#{app.id}","recordings", "1-invalid.wav")
                c.Trace application_id: app.id, step_id: 1, step_name: 'Capture number one', store: '"Invalid key pressed"'
              end
              c.Else do |c|
                c.Trace application_id: app.id, step_id: 1, step_name: 'Capture number one', store: '"No key was pressed. Timeout."'
              end
              c.Assign 'attempt_number1', 'attempt_number1 + 1'
            end
            c.Trace application_id: app.id, step_id: 1, step_name: 'Capture number one', store: '"Missed input for 3 times."'
            c.Label "end1"
          end.first
        )
      end

      it "should accept an empty 'valid_values' string and use it as 'all values are valid'" do

        capture_flow = Compiler.parse do |c|
          c.Label 4
          c.Assign "current_step", 4
          c.Assign 'attempt_number4', '1'
          c.While 'attempt_number4 <= 3' do |c|
            c.Capture min: 1, max: 1, finish_on_key: '#', timeout: 5
            c.Assign 'value_4', 'digits'
            c.If 'true' do |c|
              c.Trace application_id: app.id, step_id: 4, step_name: 'Capture', store: '"User pressed: " + digits'
              c.Goto "end4"
            end
            c.Else do |c|
              c.Trace application_id: app.id, step_id: 4, step_name: 'Capture', store: '"No key was pressed. Timeout."'
            end
            c.Assign 'attempt_number4', 'attempt_number4 + 1'
          end
          c.Trace application_id: app.id, step_id: 4, step_name: 'Capture', store: '"Missed input for 3 times."'
          c.Label "end4"
        end.first

        capture = Capture.new app,
          'id' => 4,
          'root' => true,
          'type' => 'capture',
          'name' => 'Capture'

        capture.equivalent_flow.first.should eq(capture_flow)

        capture = Capture.new app,
          'id' => 4,
          'root' => true,
          'type' => 'capture',
          'name' => 'Capture',
          'valid_values' => ''

        capture.equivalent_flow.first.should eq(capture_flow)

        capture = Capture.new app,
          'id' => 4,
          'root' => true,
          'type' => 'capture',
          'name' => 'Capture',
          'valid_values' => '   '

        capture.equivalent_flow.first.should eq(capture_flow)
      end

      it "should accept an empty input" do
         capture_flow = Compiler.parse do |c|
            c.Label 4
            c.Assign "current_step", 4
            c.Assign 'attempt_number4', '1'
            c.While 'attempt_number4 <= 3' do |c|
              c.Capture min: 0, max: 2, finish_on_key: '#', timeout: 5
              c.Assign 'value_4', 'digits'
              c.If '(digits == 1) || (digits >= 2 && digits <= 4) || (digits >= 10 && digits <= 20) || (digits == null)' do |c|
                c.Trace application_id: app.id, step_id: 4, step_name: 'Capture', store: '"User pressed: " + digits'
                c.Goto "end4"
              end
              c.Else do |c|
                c.PlayFile File.join(Rails.root, "data","applications", "#{app.id}","recordings", "4-invalid.wav")
                c.Trace application_id: app.id, step_id: 4, step_name: 'Capture', store: '"Invalid key pressed"'
              end
              c.Assign 'attempt_number4', 'attempt_number4 + 1'
            end
            c.Trace application_id: app.id, step_id: 4, step_name: 'Capture', store: '"Missed input for 3 times."'
            c.Label "end4"
          end.first

          capture = Capture.new app,
            'id' => 4,
            'root' => true,
            'type' => 'capture',
            'name' => 'Capture',
            'valid_values' => '1,2-4,10-20',
            'finish_on_key' => '#',
            'min_input_length' => 0,
            'max_input_length' => 2,
            'invalid_message' => {
              "name" => "An invalid key was pressed",
              "type" => "recording",
              "file" => "file.wav",
              "duration" => 5
            }

          capture.equivalent_flow.first.should eq(capture_flow)
      end
      it "should have a default next step" do
        capture_flow = Compiler.parse do |c|
            c.Label 4
            c.Assign "current_step", 4
            c.Assign 'attempt_number4', '1'
            c.While 'attempt_number4 <= 3' do |c|
              c.Capture min: 0, max: 2, finish_on_key: '#', timeout: 5
              c.Assign 'value_4', 'digits'
              c.If '(digits == 1) || (digits >= 2 && digits <= 4) || (digits >= 10 && digits <= 20) || (digits == null)' do |c|
                c.Trace application_id: app.id, step_id: 4, step_name: 'Capture', store: '"User pressed: " + digits'
                c.Goto "end4"
              end
              c.Else do |c|
                c.PlayFile File.join(Rails.root, "data","applications", "#{app.id}","recordings", "4-invalid.wav")
                c.Trace application_id: app.id, step_id: 4, step_name: 'Capture', store: '"Invalid key pressed"'
              end
              c.Assign 'attempt_number4', 'attempt_number4 + 1'
            end
            c.Trace application_id: app.id, step_id: 4, step_name: 'Capture', store: '"Missed input for 3 times."'
            c.Label 2
            c.Assign "current_step", 2
            c.Trace application_id: app.id, step_id: 2, step_name: 'Play', store: '"Message played."'
            c.Say "Some explanation message"
            c.Label "end4"
          end.first

          capture = Capture.new app,
            'id' => 4,
            'root' => true,
            'type' => 'capture',
            'name' => 'Capture',
            'valid_values' => '1,2-4,10-20',
            'finish_on_key' => '#',
            'min_input_length' => 0,
            'max_input_length' => 2,
            'invalid_message' => {
              "name" => "An invalid key was pressed",
              "type" => "recording",
              "file" => "file.wav",
              "duration" => 5
            },
            'default' => 2

          play = Play.new app, 'id' => 2,
            'type' => 'play',
            'name' => 'Play',
            'message' => {
              "name" => "Some explanation message",
              "type" => "text"
            }

          capture.solve_links_with [ play ]

          capture.equivalent_flow.first.should eq(capture_flow)
      end
    end
  end
end
