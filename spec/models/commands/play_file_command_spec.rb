require 'spec_helper'

module Commands
  describe PlayFileCommand do

    before(:each) do
      @file_id = 'test_file'
      @file_path = "#{Rails.root}/data/call_flows/#{@file_id}"
      @target_path = '/path/to/pbx/target_path'

      @session = Session.new :pbx => mock('pbx')
      @session.pbx.should_receive(:sound_path_for).with(@file_id).and_return(@target_path)
      @session.pbx.should_receive(:play).with(@target_path)
      @session.stub(:info)
      @session.stub(:trace)
    end

    let(:command) do
      PlayFileCommand.new @file_path
    end

    it "should convert the file if not present in target path" do
      File.should_receive(:exists?).with(@target_path).and_return(false)

      command.should_receive(:convert_to_8000_hz_gsm).with(@file_path, @target_path)
      command.run(@session)
    end

    it "should convert the file if target path is older" do
      File.should_receive(:exists?).with(@target_path).and_return(true)
      File.should_receive(:mtime).with(@target_path).and_return(Time.now.utc - 10.hours)
      File.should_receive(:mtime).with(@file_path).and_return(Time.now.utc)

      command.should_receive(:convert_to_8000_hz_gsm).with(@file_path, @target_path)
      command.run(@session)
    end

    it "should not convert the file if target path is newer" do
      File.should_receive(:exists?).with(@target_path).and_return(true)
      File.should_receive(:mtime).with(@target_path).and_return(Time.now.utc)
      File.should_receive(:mtime).with(@file_path).and_return(Time.now.utc - 10.hours)

      command.should_not_receive(:convert_to_8000_hz_gsm)
      command.run(@session)
    end

  end
end