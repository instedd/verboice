require 'spec_helper'

module Parsers
  module UserFlowNode
    describe Play do

      let(:app) { self }
      it "should compile to a verboice equivalent flow" do
        play = Play.new app, 'id' => 27,
          'type' => 'play',
          'name' => 'Play number one',
          'message' => {
            "name" => "Some explanation message",
            "type" => "recording",
            "file" => "file.wav",
            "duration" => 5
          }

        play.equivalent_flow.should eq([
          { play_file: File.join(Rails.root, "data","applications","1","recordings", "27-message.wav")}
        ])

        play2 = Play.new app, 'id' => 27,
          'type' => 'play',
          'name' => 'Play number one',
          'message' => {
            "name" => "Some explanation message",
            "type" => "text"
          }

        play2.equivalent_flow.should eq([
          { say: "Some explanation message"}
        ])
      end

      def id
        1
      end
    end
  end
end