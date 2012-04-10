require 'spec_helper'

module Parsers
  module UserFlowNode
    describe Menu do

      it "should compile to a verboice equivalent flow" do
        # todo: Add number of attempts and while cycle.
        menu = Menu.new 'id' => 27, 'type' => 'menu',
          'explanation_text' => 'foobar',
          'options_text' => 'asdasdasd',
          'timeout'=> 20,
          'number_of_attempts' => 3,
          'invalid_text' => 'invalid key pressed',
          'end_call_text' => 'Good Bye',
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
        menu_2 = Menu.new 'id' => 10, 'type' => 'menu', 'explanation_text' => 'asdf'
        menu_3 = Menu.new 'id' => 14, 'type' => 'menu', 'explanation_text' => 'qwer'
        menu_4 = Menu.new 'id' => 5, 'type' => 'menu', 'explanation_text' => 'zxcv'
        menu.solve_links_with [ menu_2, menu_3, menu_4 ]

        menu.equivalent_flow.should eq([
          { say: 'foobar' },
          { assign: { name: 'attempt_number', expr: '1' }},
          { assign: { name: 'end', expr: 'false' }},
          { :while => { :condition => 'attempt_number <= 3 && !end', :do => [
            {
              capture: {
                timeout: 20,
                say: 'asdasdasd'
              }
            },
            {
              :if => {
                :condition => "digits == 4",
                :then => [{ say: 'asdf' },
                  { assign: { name: 'end', expr: 'true' }}
                  ],
                :else => {
                  :if => {
                    :condition => "digits == 6",
                    :then => [
                      { say: 'qwer' },
                      { assign: { name: 'end', expr: 'true' }}
                    ],
                    :else => {
                      :if => {
                        :condition => "digits == 2",
                        :then => [
                          { say: 'zxcv' },
                          { assign: { name: 'end', expr: 'true' }}
                        ],
                        :else => {
                          :if => {
                            :condition => "digits != null",
                            :then => [{ say: "invalid key pressed" }]
                          }
                        }
                      }
                    }
                  }
                }
              }
            },
            { assign: { name: 'attempt_number', expr: 'attempt_number + 1' }}
          ]}},
          {
            :if => {
              :condition => 'attempt_number > 3 && !end',
              :then => [{ say: 'Good Bye' }]
            }
          }
        ])

      end

      it "should compile to a minimum verboice equivalent flow" do
        menu_1 = Menu.new 'id' => 27, 'type' => 'menu'
        menu_1.equivalent_flow.should eq([])

        menu_2 = Menu.new 'id' => 27, 'type' => 'menu',
          'explanation_text' => 'foobar',
          'options_text' => 'asdasdasd',
          'end_call_text' => 'Good Bye'

        menu_2.equivalent_flow.should eq([
          { say: 'foobar' },
          {
            say: 'asdasdasd'
          },
          {say: 'Good Bye'}
        ])

        menu_3 = Menu.new 'id' => 27, 'type' => 'menu',
          'explanation_text' => 'foobar',
          'invalid_text' => 'invalid key pressed',
          'end_call_text' => 'Good Bye',
          'options' => [
            {
              'description' => 'foo',
              'number' => 4,
              'next' => 10
            }
          ]
        menu_4 = Menu.new 'id' => 10, 'type' => 'menu', 'explanation_text' => 'asdf'
        menu_3.solve_links_with [ menu_4 ]

        menu_3.equivalent_flow.should eq([
          { say: 'foobar' },
          {:assign=>{:name=>"attempt_number", :expr=>"1"}},
          {:assign=>{:name=>"end", :expr=>"false"}},
          {:while=> {
            :condition=>"attempt_number <= 3 && !end",
            :do=> [
              {:capture=>{:timeout=>5}},
              {
                :if=> {
                  :condition=>"digits == 4",
                  :then=>[{:say=>"asdf"}, {:assign=>{:name=>"end", :expr=>"true"}}],
                  :else=> {
                    :if=> {
                      :condition=>"digits != null",
                      :then=> [{:say=>"invalid key pressed"}]
                    }
                  }
                }
              },
              {:assign=>{:name=>"attempt_number", :expr=>"attempt_number + 1"}}
            ]
          }},
          {
            :if=> {
              :condition => "attempt_number > 3 && !end",
              :then => [{:say=>"Good Bye"}]
            }
          }
        ])
      end

      it "should be able to build itself from an incomming hash" do
        menu = Menu.new 'id' => 27, 'type' => 'menu', 'explanation_text' => 'foo', 'timeout' => 20, 'invalid_text' => 'foobar', 'end_call_text' => 'cya'
        menu.id.should eq(27)
        menu.explanation_text.should eq('foo')
        menu.timeout.should eq(20)
        menu.invalid_text.should eq('foobar')
        menu.end_call_text.should eq('cya')
      end

      it "should handle a menu input stream"do
        (Menu.can_handle? 'id' => 27, 'type' => 'menu').should be_true
        (Menu.can_handle? 'id' => 27, 'type' => 'answer').should be_false
      end

      it "should build with a collection of options" do
        menu = Menu.new 'id' => 27, 'type' => 'menu', 'explanation_text' => 'foo',
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
        menu = Menu.new 'id' => 27, 'type' => 'menu', 'explanation_text' => 'foo', 'options' => [
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
        menu_2 = Menu.new 'id' => 10, 'type' => 'menu', 'explanation_text' => 'foo'
        menu_3 = Menu.new 'id' => 14, 'type' => 'menu', 'explanation_text' => 'foo'

        menu.solve_links_with [ menu_2, menu_3 ]
        menu.options[0]['next'].should eq(menu_2)
        menu.options[1]['next'].should eq(menu_3)
      end

      it "should respond if it's a root or not" do
        menu_1 = Menu.new 'id' => 10, 'root' => true, 'type' => 'menu', 'explanation_text' => 'foo'
        menu_2 = Menu.new 'id' => 14, 'type' => 'menu', 'explanation_text' => 'foo'
        menu_1.is_root?.should be_true
        menu_2.is_root?.should be_false
      end
    end
  end
end