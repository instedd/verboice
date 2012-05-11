require 'spec_helper'

describe Parsers::UserFlow do
  let(:application) do
    app = mock('application')
    app.stubs(:id).returns 5
    app
  end

  let (:application_flow) do
    [
      {
        'id' => 27,
        'root' => 2,
        'type' => 'play',
        'name' => 'Play number 27',
        'message' => {
          "name" => "Some explanation message",
          "type" => "recording",
          "file" => "file.wav",
          "duration" => 5
        }
      },
      {
        'id' => 3,
        'type' => 'menu',
        'name' => 'Menu number one',
        'explanation_message' => { "name" => 'First Menu', 'type' => 'text' },
        'options_message' => {},
        'invalid_message' => {},
        'timeout' => 20,
        'options' =>[
          {
            'description' => 'foobar',
            'number' => 2,
            'next' => 4
          },
          {
            'description' => 'Some other option',
            'number' => 1,
            'next' => 6
          }
        ],
        'next' => 5
      },
      {
        'id' => 4,
        'type' => 'play',
        'name' => 'Say number 4',
        'message' => {
          "name" => "Say 4",
          "type" => "text"
        }
      },
      {
        'id' => 1,
        'root' => 1,
        'type' => 'play',
        'name' => 'Play number one',
        'message' => {
          "name" => "Some explanation message",
          "type" => "recording",
          "file" => "file.wav",
          "duration" => 5
        },
        'next' => 2
      },
      {
        'id' => 2,
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
        'valid_values' => '1-10',
        'finish_on_key' => '#',
        'min_input_length' => 1,
        'max_input_length' => 10,
        'timeout' => 10,
        'next' => 3
      },
      {
        'id' => 6,
        'type' => 'play',
        'name' => 'Say number 6',
        'message' => {
          "name" => "Say 6",
          "type" => "text"
        }
      },
      {
        'id' => 5,
        'type' => 'play',
        'name' => 'Say number 5',
        'message' => {
          "name" => "Say 5",
          "type" => "text"
        },
        'next' => 7
      },
      {
        'id' => 7,
        'type' => 'goto',
        'jump' => 33
      },
      {
        'id' => 33,
        'root' => 3,
        'type' => 'play',
        'name' => 'Play number 33',
        'message' => {
          "name" => "Some explanation message",
          "type" => "recording",
          "file" => "file.wav",
          "duration" => 5
        },
        'next' => 34
      },
      {
        'id' => 34,
        'type' => 'branch',
        'name' => 'Branch number one',
        'options' => [
          {
            'description' => 'foo',
            'conditions' => [
              {
                'step' => 3,
                'operator' => '==',
                'value' => 6
              },
              {
                'step' => 2,
                'operator' => '<',
                'value' => 30
              },
              {
                'step' => 2,
                'operator' => '>=',
                'value' => 5
              }
            ],
            'next' => 10
          },
          {
            'description' => 'bar',
            'conditions' => [
              {
                'step' => 3,
                'operator' => '<=',
                'value' => 5
              }
            ],
            'next' => 14
          }
        ]
      },
      {
        'id' => 10,
        'type' => 'play',
        'name' => 'Play number 10',
        'message' => {
          "name" => "Some explanation message",
          "type" => "recording",
          "file" => "file.wav",
          "duration" => 5
        }
      },
      {
        'id' => 14,
        'type' => 'play',
        'name' => 'Say 14',
        'message' => {
          "name" => "Say 14",
          "type" => "text"
        },
        'next' => 15
      },
      {
        'id' => 15,
        'name' => 'Hanged up!',
        'type' => 'hang_up'
      },
      {
        'id' => 44,
        'root' => 4,
        'type' => 'play',
        'name' => 'Play number 44',
        'message' => {
          "name" => "Some explanation message",
          "type" => "recording",
          "file" => "file.wav",
          "duration" => 5
        }
      }
    ]
  end

  it "should retrieve an equivalent flow in verboice internal representation" do
    (Parsers::UserFlow.new application, application_flow).equivalent_flow.should eq(
      Compiler.make do
        Answer()
        Assign "current_step", 1
        Trace application_id: 5, step_id: 1, step_name: 'Play number one', store: '"Message played."'
        PlayFile File.join(Rails.root, "data","r_spec","mocks","mocks","5","recordings", "1-message.wav")
        Assign "current_step", 2
        Assign 'attempt_number2', '1'
        While 'attempt_number2 <= 3' do
          Capture say: "First Capture", min: 1, max: 10, finish_on_key: '#', timeout: 10
          Assign 'value_2', 'digits'
          If "(digits >= 1 && digits <= 10)" do
            Trace application_id: 5, step_id: 2, step_name: 'Capture number one', store: '"User pressed: " + digits'
            Goto "end2"
          end
          If "digits != null" do
            PlayFile File.join(Rails.root, "data","r_spec","mocks","mocks","5","recordings", "2-invalid.wav")
            Trace application_id: 5, step_id: 2, step_name: 'Capture number one', store: '"Invalid key pressed"'
          end
          Else do
            Trace application_id: 5, step_id: 2, step_name: 'Capture number one', store: '"No key was pressed. Timeout."'
          end
          Assign 'attempt_number2', 'attempt_number2 + 1'
        end
        Trace application_id: 5, step_id: 2, step_name: 'Capture number one', store: '"Missed input for 3 times."'
        Label "end2"
        Assign "current_step", 3
        Say 'First Menu'
        Assign 'attempt_number3', '1'
        While 'attempt_number3 <= 3' do
          Capture finish_on_key: '', timeout: 20
          Assign 'value_3', 'digits'
          If "digits == '2'" do
            Trace application_id: 5, step_id: 3, step_name: 'Menu number one', store: '"User pressed: " + digits'
            Assign "current_step", 4
            Trace application_id: 5, step_id: 4, step_name: 'Say number 4', store: '"Message played."'
            Say "Say 4"
            Goto "end3"
          end
          If "digits == '1'" do
            Trace application_id: 5, step_id: 3, step_name: 'Menu number one', store: '"User pressed: " + digits'
            Assign "current_step", 6
            Trace application_id: 5, step_id: 6, step_name: 'Say number 6', store: '"Message played."'
            Say "Say 6"
            Goto "end3"
          end
          If "digits != null" do
            Trace application_id: 5, step_id: 3, step_name: 'Menu number one', store: '"Invalid key pressed"'
          end
          Else do
            Trace application_id: 5, step_id: 3, step_name: 'Menu number one', store: '"No key was pressed. Timeout."'
          end
          Assign 'attempt_number3', 'attempt_number3 + 1'
        end
        Trace application_id: 5, step_id: 3, step_name: 'Menu number one', store: '"Missed input for 3 times."'
        Label "end3"
        Assign "current_step", 5
        Trace application_id: 5, step_id: 5, step_name: 'Say number 5', store: '"Message played."'
        Say "Say 5"
        Assign "current_step", 33
        Trace application_id: 5, step_id: 33, step_name: 'Play number 33', store: '"Message played."'
        PlayFile File.join(Rails.root, "data","r_spec","mocks","mocks","5","recordings", "33-message.wav")
        Assign "current_step", 34
        If "(value_3 == 6) && (value_2 < 30) && (value_2 >= 5)" do
          Trace application_id: 5, step_id: 34, step_name: 'Branch number one', store: '"Branch number 1 selected: \'foo\'"'
          Label 10
          Assign "current_step", 10
          Trace application_id: 5, step_id: 10, step_name: 'Play number 10', store: '"Message played."'
          PlayFile File.join(Rails.root, "data","r_spec","mocks","mocks","5","recordings", "10-message.wav")
          Goto "end34"
        end
        If "(value_3 <= 5)" do
          Trace application_id: 5, step_id: 34, step_name: 'Branch number one', store: '"Branch number 2 selected: \'bar\'"'
          Label 14
          Assign "current_step", 14
          Trace application_id: 5, step_id: 14, step_name: 'Say 14', store: '"Message played."'
          Say "Say 14"
          Label 15
          Assign "current_step", 15
          Trace application_id: 5, step_id: 15, step_name: 'Hanged up!', store: '"Application ended call."'
          End()
          Goto "end34"
        end
        Trace(application_id: 5, step_id: 34, step_name: 'Branch number one', store: '"No branch was selected."')
        Label "end34"
      end
    )
  end

  it "should provide a hash of step names and IDs" do
    (Parsers::UserFlow.new application, application_flow).step_names.should eq({
      27 => "Play number 27",
      3  => 'Menu number one',
      4  => 'Say number 4',
      1  => "Play number one",
      2  => "Capture number one",
      6  => "Say number 6",
      5  => "Say number 5",
      33 => "Play number 33",
      34 => "Branch number one",
      10 => "Play number 10",
      14 => "Say 14",
      15 => "Hanged up!",
      44 => "Play number 44"
    })
  end

  it "should provide an error flow to append to a given application" do
    (Parsers::UserFlow.new application, application_flow).error_flow.should eq(
      Compiler.make do
        Trace application_id: 5, step_id: 'current_step', step_name: '', store: '"User hanged up."'
      end
    )
  end

end
