class AddUploadedAudioToLocalizedResource < ActiveRecord::Migration
  def change
    add_column :localized_resources, :uploaded_audio, :binary, :limit => 16.megabyte
    rename_column :localized_resources, :audio, :recorded_audio
  end
end
