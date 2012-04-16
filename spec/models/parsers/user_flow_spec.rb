require 'spec_helper'

describe Parsers::UserFlow do
  let(:application) do
    app = mock('application')
    app.stubs(:id).returns 1
    app
  end

  let (:application_flow) do
    [
      {
        'id' => 3,
        'type' => 'menu',
        'name' => 'Menu number one',
        'explanation_message' => { "name" => 'First Menu', 'type' => 'text' },
        'options_message' => {},
        'end_call_message' => {},
        'invalid_message' => {},
        'options' =>[
          {
            'description' => 'foobar',
            'number' => 2,
            'next' => 4
          }
        ]
      },
      {
        'id' => 4,
        'type' => 'menu',
        'name' => 'Menu number two',
        'explanation_message' => {"name" => 'Second Menu', 'type' => 'text'},
        'options_message' => {},
        'end_call_message' => {},
        'invalid_message' => {}
      },
      {
        'id' => 1,
        'root' => true,
        'type' => 'play',
        'name' => 'Play number one',
        'root' => 'true',
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
        'valid_values' => '1,2-4,10-20',
        'finish_on_key' => '#',
        'min_input_value' => 1,
        'max_input_value' => 10,
        'timeout' => 10,
        'next' => 3
      },
      {
        'id' => 27,
        'root' => true,
        'type' => 'play',
        'name' => 'Play number 27',
        'root' => 'true',
        'message' => {
          "name" => "Some explanation message",
          "type" => "recording",
          "file" => "file.wav",
          "duration" => 5
        }
      }
    ]
  end

  it "should deliver a collection of parser nodes" do
    nodes = (Parsers::UserFlow.new application, application_flow[0...-1]).build_nodes
    nodes.size.should eq(1)
    play = nodes.first
    play.class.should eq(Parsers::UserFlowNode::Play)
    play.id.should eq(1)
    capture = play.next
    capture.class.should eq(Parsers::UserFlowNode::Capture)
    capture.id.should eq(2)
    first_menu = capture.next
    first_menu.class.should eq(Parsers::UserFlowNode::Menu)
    first_menu.id.should eq(3)
    second_menu = first_menu.options.first['next']
    second_menu.class.should eq(Parsers::UserFlowNode::Menu)
    second_menu.id.should eq(4)
    second_menu.options.size.should eq(0)
  end

  it "should retrieve an equivalent flow in verboice internal representation" do
    (Parsers::UserFlow.new application, application_flow).equivalent_flow.should eq([
      [
        { :play_file=> "/Users/nekron/Projects/verboice/verboice/data/applications/1/recordings/1-message.wav" },
        { assign: { name: 'attempt_number2', expr: '1' }},
        { assign: { name: 'end2', expr: 'false' }},
        { :while => { :condition => 'attempt_number2 <= 3 && !end2', :do => [
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
                  :step_id => 2,
                  :step_name => 'Capture number one',
                  :store => '"User pressed: " + digits'
                }},
                { :say => "First Menu" },
                { :assign => { :name => "attempt_number3", :expr=>"1" }},
                { :assign => { :name => "end3", :expr=>"false" }},
                { :while => {
                  :condition => "attempt_number3 <= 3 && !end3",
                  :do=> [
                    { :capture => { :timeout=>5 }},
                    { :if => {
                      :condition => "digits == 2",
                      :then => [
                        { :trace => {
                          :application_id => 1,
                          :step_id => 3,
                          :step_name => 'Menu number one',
                          :store => "\"User pressed: \" + digits"
                        }},
                        { :say => "Second Menu" },
                        {
                          :trace => {
                            :application_id=>1,
                            :step_id=> 4,
                            :step_name=>"Menu number two",
                            :store=>"\"Call ended.\""
                          }
                        },
                        { :assign => { :name=>"end3", :expr=>"true" }}
                      ],
                      :else => {
                        :if => {
                          :condition => "digits != null",
                          :then => [
                            { :trace => {
                              :application_id => 1,
                              :step_id => 3,
                              :step_name => 'Menu number one',
                              :store => "\"Invalid key pressed\""
                            }}
                          ],
                          :else => [
                            { :trace => {
                              :application_id => 1,
                              :step_id => 3,
                              :step_name => 'Menu number one',
                              :store => "\"No key was pressed. Timeout.\""
                            }}
                          ]
                        }
                      }
                    }},
                    { :assign => { :name => "attempt_number3", :expr => "attempt_number3 + 1" }}
                  ]
                }},
                {
                  :trace => {
                    :application_id=>1,
                    :step_id=>3,
                    :step_name=>"Menu number one",
                    :store=>"\"Call ended.\""
                  }
                },
                { assign: { name: 'end2', expr: 'true' }}
              ],
              :else => {
                :if => {
                  :condition => "digits != null",
                  :then => [
                    { play_file: File.join(Rails.root, "data","applications","1","recordings", "2-invalid.wav")},
                    { trace: {
                      :application_id => 1,
                      :step_id => 2,
                      :step_name => 'Capture number one',
                      :store => '"Invalid key pressed"'
                    }}
                  ],
                  :else => [
                    { trace: {
                      :application_id => 1,
                      :step_id => 2,
                      :step_name => 'Capture number one',
                      :store => '"No key was pressed. Timeout."'
                    }}
                  ]
                }
              }
            }
          },
          { assign: { name: 'attempt_number2', expr: 'attempt_number2 + 1' }}
        ]}},
        {
          :if => {
            :condition => 'attempt_number2 > 3 && !end2',
            :then => [
              { play_file: File.join(Rails.root, "data","applications","1","recordings", "2-end_call.wav")},
              { trace: {
                :application_id => 1,
                :step_id => 2,
                :step_name => 'Capture number one',
                :store => '"Missed input for 3 times."'
              }}
            ]
          }
        },
        {
          :trace=> {
            :application_id => 1,
            :step_id => 2,
            :step_name => "Capture number one",
            :store => '"Call ended."'
          }
        }
      ],
      [
        { :play_file=> "/Users/nekron/Projects/verboice/verboice/data/applications/1/recordings/27-message.wav" }
      ]
    ])
  end

  it "should provide a hash of step names and IDs" do
    (Parsers::UserFlow.new application, application_flow).step_names.should eq({
      3 => 'Menu number one',
      4 => 'Menu number two',
      1  => "Play number one",
      2  => "Capture number one",
      27 => "Play number 27"
    })
  end

end