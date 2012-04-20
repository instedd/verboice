require 'spec_helper'

module Parsers
  module UserFlowNode
    describe Play do

      let(:app) { self }

      it "should compile to a verboice equivalent flow" do
        play = Play.new app, 'id' => 1,
          'type' => 'play',
          'name' => 'Play',
          'message' => {
            "name" => "Some explanation message",
            "type" => "recording",
            "file" => "file.wav",
            "duration" => 5
          }

        play.equivalent_flow.first.should eq(
          Compiler.parse do
            Label 1
            Trace application_id: 1, step_id: 1, step_name: 'Play', store: '"Message played."'
            PlayFile File.join(Rails.root, "data","applications","1","recordings", "1-message.wav")
          end.first
        )
      end
      it "should compile a tts message as well" do

        play = Play.new app, 'id' => 27,
          'type' => 'play',
          'name' => 'Play number one',
          'message' => {
            "name" => "Some explanation message",
            "type" => "text"
          }

        play.equivalent_flow.first.should eq(
          Compiler.parse do
            Label 27
            Trace application_id: 1, step_id: 27, step_name: 'Play number one', store: '"Message played."'
            Say "Some explanation message"
          end.first
        )
      end

      def id
        1
      end
    end
  end
end