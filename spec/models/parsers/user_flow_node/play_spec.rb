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
      end

      def id
        1
      end
    end
  end
end