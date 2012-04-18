require 'spec_helper'

module Parsers
  module UserFlowNode
    describe Capture do

      let(:app) { self }

      it "should compile to a verboice equivalent flow" do
        capture = Capture.new app,
          'id' => 1,
          'root' => true,
          'type' => 'capture',
          'name' => 'Capture number one',
          'instructions_message' => { "name" => 'First Capture', 'type' => 'text' },
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
          'valid_values' => '1,2-4,10-20',
          'finish_on_key' => '#',
          'min_input_length' => 1,
          'max_input_length' => 2,
          'timeout' => 10

        capture.equivalent_flow.should eq(
          Compiler.make do
            Assign 'attempt_number1', '1'
            While 'attempt_number1 <= 3' do
              Capture say: "First Capture", min: 1, max: 2, finish_on_key: '#', timeout: 10
              If "(digits == 1) || (digits >= 2 && digits <= 4) || (digits >= 10 && digits <= 20)" do
                Trace application_id: 1, step_id: 1, step_name: 'Capture number one', store: '"User pressed: " + digits'
                Goto "end1"
              end
              If "digits != null" do
                PlayFile File.join(Rails.root, "data","applications","1","recordings", "1-invalid.wav")
                Trace application_id: 1, step_id: 1, step_name: 'Capture number one', store: '"Invalid key pressed"'
              end
              Else do
                Trace application_id: 1, step_id: 1, step_name: 'Capture number one', store: '"No key was pressed. Timeout."'
              end
              Assign 'attempt_number1', 'attempt_number1 + 1'
            end
            Trace application_id: 1, step_id: 1, step_name: 'Capture number one', store: '"Missed input for 3 times."'
            PlayFile File.join(Rails.root, "data","applications","1","recordings", "1-end_call.wav")
            Label "end1"
          end
        )
      end

      def id
        1
      end
    end
  end
end
