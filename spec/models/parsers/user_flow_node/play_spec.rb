require 'spec_helper'

module Parsers
  module UserFlowNode
    describe Play do

      let(:call_flow) { CallFlow.make }

      it "should compile to a verboice equivalent flow" do
        play = Play.new call_flow, 'id' => 1,
          'type' => 'play',
          'name' => 'Play',
          'message' => {
            "name" => "Some explanation message",
            "type" => "recording",
            "file" => "file.wav",
            "duration" => 5
          }

        play.equivalent_flow.first.should eq(
          Compiler.parse do |c|
            c.Label 1
            c.Assign "current_step", 1
            c.Trace call_flow_id: call_flow.id, step_id: 1, step_name: 'Play', store: '"Message played."'
            c.PlayFile File.join(Rails.root, "data","call_flows","#{call_flow.id}","recordings", "1-message.wav")
          end.first
        )
      end
      it "should compile a tts message as well" do

        play = Play.new call_flow, 'id' => 27,
          'type' => 'play',
          'name' => 'Play number one',
          'message' => {
            "name" => "Some explanation message",
            "type" => "text"
          }

        play.equivalent_flow.first.should eq(
          Compiler.parse do |c|
            c.Label 27
            c.Assign "current_step", 27
            c.Trace call_flow_id: call_flow.id, step_id: 27, step_name: 'Play number one', store: '"Message played."'
            c.Say "Some explanation message"
          end.first
        )
      end
    end
  end
end