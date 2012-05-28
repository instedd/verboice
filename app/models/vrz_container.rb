class VrzContainer

  def initialize(project)
    @project = project
    @recoding_manager = RecordingManager.for(@project)
  end

  def self.for(project)
    self.new(project)
  end

  def import path
    Zip::ZipFile.open(path) do |zip|
      zip.each do |entry|
        ext = File.extname entry.name
        case ext
        when '.yml'
          @project.user_flow = YAML::load(zip.read(entry))
        when '.wav'
          zip.extract(entry, File.join(@recoding_manager.recordings_folder, entry.name)) {true} # true to always overwrite
        end
      end
    end
  end

  def export path
    Zip::ZipOutputStream.open(path) do |zos|
      # workflow
      zos.put_next_entry 'workflow.yml'
      zos.print @project.user_flow.to_yaml
      # audio files
      Dir.glob(File.join(@recoding_manager.recordings_folder, '*.wav')) do |audio_file|
        zos.put_next_entry File.basename(audio_file)
        zos.print IO.read(audio_file)
      end
    end
  end

end