class ChangeLocalizedResourceAudioToLongBlob < ActiveRecord::Migration
  def up
    change_column :localized_resources, :audio, :binary, :limit => 16.megabyte
  end

  def down
    change_column :localized_resources, :audio, :binary
  end
end
