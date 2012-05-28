require 'spec_helper'

describe VrzContainer do

  let(:project) { Project.new :user_flow => 'the user flow' }
  let(:recording_manager) { double('recording_manager') }
  let(:vrb_container) { VrzContainer.for project }
  let(:path) { '/path/to/zip' }

  before(:each) do
    RecordingManager.stub(:for).with(project).and_return(recording_manager)
    recording_manager.stub(:recordings_folder).and_return('/path/to/recordings')
  end

  it 'exports' do
    # Mock zip open
    zip_block = nil
    Zip::ZipOutputStream.should_receive(:open) do |the_path, &block|
      zip_block = block
      the_path.should eq(path)
    end

    # Mock audio files
    dir_block = nil
    Dir.should_receive(:glob).with('/path/to/recordings/*.wav') do |&block|
      dir_block = block
    end

    # Mock zip stream
    stream = double('stream')
    stream.should_receive(:put_next_entry).with('workflow.yml')
    stream.should_receive(:print).with(project.user_flow.to_yaml)

    stream.should_receive(:put_next_entry).with('one.wav')
    stream.should_receive(:print).with('one.wav content')

    stream.should_receive(:put_next_entry).with('two.wav')
    stream.should_receive(:print).with('two.wav content')

    vrb_container.export path
    zip_block.call stream

    %w(one.wav two.wav).each do |audio_name|
      audio_path = "/path/to/recordings/#{audio_name}"
      IO.should_receive(:read).with(audio_path).and_return("#{audio_name} content")
      dir_block.call audio_path
    end
  end

  it 'imports' do
    # Mock zip open
    zip_block = nil
    Zip::ZipFile.should_receive(:open) do |the_path, &block|
      zip_block = block
      the_path.should eq(path)
    end

    # Mock entries
    zip = double('zip')
    each_block = nil
    zip.should_receive(:each) do |&block|
      each_block = block
    end

    vrb_container.import path
    zip_block.call zip

    # workflow entry
    workflow_entry = double('workflow', :name => 'workflow.yml')
    zip.should_receive(:read).with(workflow_entry).and_return('updated user flow'.to_yaml)
    each_block.call workflow_entry
    project.user_flow.should eq('updated user flow')

    # audio entries
    %w(one.wav two.wav).each do |audio_name|
      audio_entry = double("audio_entry_for_#{audio_name}", :name => audio_name)
      zip.should_receive(:extract).with(audio_entry, "/path/to/recordings/#{audio_name}")
      each_block.call audio_entry
    end
  end

end