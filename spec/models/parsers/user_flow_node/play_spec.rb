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

        play.equivalent_flow.should eq(
        Commands::PlayFileCommand.new File.join(Rails.root, "data","applications","1","recordings", "27-message.wav"))
      end
      it "should compile a tts message as well" do

        play = Play.new app, 'id' => 27,
          'type' => 'play',
          'name' => 'Play number one',
          'message' => {
            "name" => "Some explanation message",
            "type" => "text"
          }

        play.equivalent_flow.should eq( Commands::SayCommand.new "Some explanation message" )
      end

      def id
        1
      end
    end
  end
end