require 'spec_helper'

module Parsers
  module UserFlowNode
    describe Record do

      let(:app) { double('app', :id => 5) }
      let(:rm) { RecordingManager.for(app) }
      let(:url_options) { {:host => 'test.com'} }

      before :all do
        Rails.application.config.default_url_options = url_options
      end

      it "should compile to an equivalent flow" do
        record = Record.new app, 'id' => 1,
          'type' => 'record',
          'name' => 'Record Step',
          'explanation_message' => {
            "name" => "Explanation message",
            "type" => "text"
          },
          'confirmation_message' => {
            "name" => "Confirmation message",
            "type" => "text"
          },
          'timeout' => 7,
          'stop_key' => '#'

        filename = rm.get_result_path_for(1)
        record_url = Rails.application.routes.url_helpers.result_application_url(5, url_options.merge(:step_id => 1))

        record.equivalent_flow.first.should eq(
          Compiler.parse do
            Label 1
            Assign "current_step", 1
            Trace application_id: 5, step_id: 1, step_name: 'Record Step', store: %("Record message. Download link: #{record_url}")
            Say "Explanation message"
            Record filename, {:stop_keys => '#', :timeout => 7}
            Say "Confirmation message"
          end.first
        )
      end

    end
  end
end