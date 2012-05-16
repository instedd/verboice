require 'spec_helper'

module Parsers
  module UserFlowNode
    describe Branch do

      let(:project) { self }
      it "should compile to a verboice equivalent flow" do
        branch = Branch.new project, 'id' => 1,
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
                  'variable' => 'some_name',
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
                  'step' => 1,
                  'operator' => '<=',
                  'rhs_variable' => 'another_name',
                },
                {
                  'variable' => 'some_name',
                  'operator' => '<=',
                  'rhs_variable' => 'another_name',
                }
              ],
              'next' => 14
            },
            {
              'description' => 'zzz',
              'next' => 5
            }
          ]
        play1 = Play.new project, 'id' => 10,
          'type' => 'play',
          'name' => 'Play 1',
          'message' => {
            "name" => "Second explanation message",
            "type" => "text"
          }
        play2 = Play.new project, 'id' => 14,
          'type' => 'play',
          'name' => 'Play 2',
          'message' => {
            "name" => "Third explanation message",
            "type" => "text"
          }
        play3 = Play.new project, 'id' => 5,
          'type' => 'play',
          'name' => 'Play 3',
          'message' => {
            "name" => "Fourth explanation message",
            "type" => "text"
          }

        branch.solve_links_with [ play1, play2, play3 ]

        branch.equivalent_flow.first.should eq(
          Compiler.parse do
            Label 1
            Assign "current_step", 1
            RetrieveVariable 'some_name'
            If "(value_3 == 6) && (var_some_name >= 5)" do
              Trace project_id: 1, step_id: 1, step_name: 'Branch number one', store: '"Branch number 1 selected: \'foo\'"'
              Label 10
              Assign "current_step", 10
              Trace project_id: 1, step_id: 10, step_name: 'Play 1', store: '"Message played."'
              Say "Second explanation message"
              Goto "end1"
            end
            RetrieveVariable 'another_name'
            RetrieveVariable 'some_name'
            If "(value_1 <= var_another_name) && (var_some_name <= var_another_name)" do
              Trace project_id: 1, step_id: 1, step_name: 'Branch number one', store: '"Branch number 2 selected: \'bar\'"'
              Label 14
              Assign "current_step", 14
              Trace project_id: 1, step_id: 14, step_name: 'Play 2', store: '"Message played."'
              Say "Third explanation message"
              Goto "end1"
            end
            If "true" do
              Trace project_id: 1, step_id: 1, step_name: 'Branch number one', store: '"Branch number 3 selected: \'zzz\'"'
              Label 5
              Assign "current_step", 5
              Trace project_id: 1, step_id: 5, step_name: 'Play 3', store: '"Message played."'
              Say "Fourth explanation message"
              Goto "end1"
            end
            Trace(project_id: 1, step_id: 1, step_name: 'Branch number one', store: '"No branch was selected."')
            Label "end1"
          end.first
        )
      end

      it "should handle a branch input stream"do
        (Branch.can_handle? 'id' => 27, 'type' => 'branch').should be_true
        (Branch.can_handle? 'id' => 27, 'type' => 'answer').should be_false
      end

      it "should resolve it's next links from a given list of commands" do
        branch = Branch.new project, 'id' => 27, 'type' => 'branch',
          'explanation_message' => {"name" => 'foo', 'type' => 'text'},
          'options' =>[
            {
              'description' => 'foo',
              'next' => 10
            },
            {
              'description' =>'bar',
              'next' => 14
            }
          ]
        play_1 = Branch.new project, 'id' => 10,
          'type' => 'play',
          'message' => {"name"=>'foo', 'type' => 'text'}
        play_2 = Play.new project, 'id' => 14,
          'type' => 'play',
          'message' => {"name"=>'foo', 'type' => 'text'}

        branch.solve_links_with [ play_1, play_2 ]
        branch.options[0]['next'].should eq(play_1)
        branch.options[1]['next'].should eq(play_2)
      end

      it "should respond if it's a root or not" do
        branch_1 = Branch.new project, 'id' => 10,
          'root' => 1,
          'type' => 'branch',
          'explanation_message' => {"name"=>'foo', 'type' => 'text'}
        branch_2 = Branch.new project, 'id' => 14,
          'type' => 'branch',
          'explanation_message' => {"name"=>'foo', 'type' => 'text'}
        branch_1.is_root?.should be_true
        branch_2.is_root?.should be_false
      end

      def id
        1
      end
    end
  end
end