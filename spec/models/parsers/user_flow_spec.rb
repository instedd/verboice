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
        'id' => 12,
        'root' => true,
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
            'next' => 14
          }
        ]
      },
      {
        'id' => 14,
        'type' => 'menu',
        'name' => 'Menu number two',
        'explanation_message' => {"name" => 'Second Menu', 'type' => 'text'},
        'options_message' => {},
        'end_call_message' => {},
        'invalid_message' => {}
      }
    ]
  end

  it "should deliver a collection of parser nodes" do
    nodes = (Parsers::UserFlow.new application, application_flow).build_nodes
    nodes.size.should eq(1)
    first_menu = nodes.first
    first_menu.class.should eq(Parsers::UserFlowNode::Menu)
    first_menu.id.should eq(12)
    first_menu.explanation_message.name.should eq('First Menu')
    first_menu.options.size.should eq(1)
    first_menu.options.first['number'].should eq(2)
    first_menu.options.first['description'].should eq('foobar')
    first_menu.application.should eq(application)
    second_menu = first_menu.options.first['next']
    second_menu.class.should eq(Parsers::UserFlowNode::Menu)
    second_menu.id.should eq(14)
    second_menu.explanation_message.name.should eq('Second Menu')
    second_menu.options.size.should eq(0)
    second_menu.application.should eq(application)
  end

  it "should retrieve an equivalent flow in verboice internal representation" do
    (Parsers::UserFlow.new application, application_flow).equivalent_flow.should eq([
      { :say => "First Menu" },
      { :assign => { :name => "attempt_number", :expr=>"1" }},
      { :assign => { :name => "end", :expr=>"false" }},
      { :while => {
        :condition => "attempt_number <= 3 && !end",
        :do=> [
          { :capture => { :timeout=>5 }},
          { :if => {
            :condition => "digits == 2",
            :then => [
              { :trace => {
                :application_id => 1,
                :step_id => 12,
                :step_name => 'Menu number one',
                :store => "\"User pressed: \" + digits"
              }},
              { :say => "Second Menu" },
              {
                :trace => {
                  :application_id=>1,
                  :step_id=>14,
                  :step_name=>"Menu number two",
                  :store=>"\"Call ended.\""
                }
              },
              { :assign => { :name=>"end", :expr=>"true" }}
            ],
            :else => {
              :if => {
                :condition => "digits != null",
                :then => [
                  { :trace => {
                    :application_id => 1,
                    :step_id => 12,
                    :step_name => 'Menu number one',
                    :store => "\"Invalid key pressed\""
                  }}
                ],
                :else => [
                  { :trace => {
                    :application_id => 1,
                    :step_id => 12,
                    :step_name => 'Menu number one',
                    :store => "\"No key was pressed. Timeout.\""
                  }}
                ]
              }
            }
          }},
          { :assign => { :name => "attempt_number", :expr => "attempt_number + 1" }}
        ]
      }},
      {
        :trace => {
          :application_id=>1,
          :step_id=>12,
          :step_name=>"Menu number one",
          :store=>"\"Call ended.\""
        }
      }
    ])
  end

  it "should provide a hash of step names and IDs" do
    (Parsers::UserFlow.new application, application_flow).step_names.should eq({ 12 => 'Menu number one', 14 => 'Menu number two' })
  end

end