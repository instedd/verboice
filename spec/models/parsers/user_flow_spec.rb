require 'spec_helper'

describe Parsers::UserFlow do

  let (:application_flow) do
    [
      {
        'id' => 12,
        'root' => true,
        'type' => 'menu',
        'explanation_text' => 'First Menu',
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
        'explanation_text' => 'Second Menu'
      }
    ]
  end

  it "should deliver a collection of parser nodes" do
    nodes = (Parsers::UserFlow.new application_flow).build_nodes
    nodes.size.should eq(1)
    first_menu = nodes.first
    first_menu.class.should eq(Parsers::UserFlowNode::Menu)
    first_menu.id.should eq(12)
    first_menu.explanation_text.should eq('First Menu')
    first_menu.options.size.should eq(1)
    first_menu.options.first['number'].should eq(2)
    first_menu.options.first['description'].should eq('foobar')
    second_menu = first_menu.options.first['next']
    second_menu.class.should eq(Parsers::UserFlowNode::Menu)
    second_menu.id.should eq(14)
    second_menu.explanation_text.should eq('Second Menu')
    second_menu.options.size.should eq(0)
  end

  it "should retrieve an equivalent flow in verboice internal representation" do
    (Parsers::UserFlow.new application_flow).equivalent_flow.should eq([
      {:say=>"First Menu"},
      {:assign=>{:name=>"attempt_number", :expr=>"1"}},
      {:assign=>{:name=>"end", :expr=>"false"}},
      {:while=>
        {:condition=>"attempt_number <= 3 && !end",
         :do=>
          [{:capture=>{:timeout=>5}},
           {:if=>
             {:condition=>"digits == 2",
              :then=> [
                {:say=>"Second Menu"},
                {:assign=>{:name=>"end", :expr=>"true"}}
              ]}},
           {:assign=>{:name=>"attempt_number", :expr=>"attempt_number + 1"}}]}}
    ])
  end
end