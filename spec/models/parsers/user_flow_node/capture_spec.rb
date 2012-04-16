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
          'min_input_value' => 1,
          'max_input_value' => 10,
          'timeout' => 10

        capture.equivalent_flow.should eq([
          { assign: { name: 'attempt_number1', expr: '1' }},
          { assign: { name: 'end1', expr: 'false' }},
          { :while => { :condition => 'attempt_number1 <= 3 && !end1', :do => [
            {
              capture: {
                say: "First Capture",
                timeout: 10
              }
            },
            {
              :if => {
                :condition => "digits >= 1 && digits <= 10",
                :then => [
                  { trace: {
                    :application_id => 1,
                    :step_id => 1,
                    :step_name => 'Capture number one',
                    :store => '"User pressed: " + digits'
                  }},
                  { assign: { name: 'end1', expr: 'true' }}
                ],
                :else => {
                  :if => {
                    :condition => "digits != null",
                    :then => [
                      { play_file: File.join(Rails.root, "data","applications","1","recordings", "1-invalid.wav")},
                      { trace: {
                        :application_id => 1,
                        :step_id => 1,
                        :step_name => 'Capture number one',
                        :store => '"Invalid key pressed"'
                      }}
                    ],
                    :else => [
                      { trace: {
                        :application_id => 1,
                        :step_id => 1,
                        :step_name => 'Capture number one',
                        :store => '"No key was pressed. Timeout."'
                      }}
                    ]
                  }
                }
              }
            },
            { assign: { name: 'attempt_number1', expr: 'attempt_number1 + 1' }}
          ]}},
          {
            :if => {
              :condition => 'attempt_number1 > 3 && !end1',
              :then => [
                { play_file: File.join(Rails.root, "data","applications","1","recordings", "1-end_call.wav")},
                { trace: {
                  :application_id => 1,
                  :step_id => 1,
                  :step_name => 'Capture number one',
                  :store => '"Missed input for 3 times."'
                }}
              ]
            }
          },
          {
            :trace=> {
              :application_id => 1,
              :step_id => 1,
              :step_name => "Capture number one",
              :store => '"Call ended."'
            }
          }
        ])
      end

      def id
        1
      end
    end
  end
end