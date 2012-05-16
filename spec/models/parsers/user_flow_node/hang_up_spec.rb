require 'spec_helper'

module Parsers
  module UserFlowNode
    describe HangUp do

      let(:project) { self }

      it "should compile to a verboice equivalent flow" do
        hang_up = HangUp.new project, 'id' => 1,
          'type' => 'hang_up',
          'name' => 'Hang up'

        hang_up.equivalent_flow.first.should eq(
          Compiler.parse do
            Label 1
            Assign "current_step", 1
            Trace project_id: 1, step_id: 1, step_name: 'Hang up', store: '"Verboice ended call."'
            End()
          end.first
        )
      end

      def id
        1
      end
    end
  end
end