class AddTimeZoneToProjects < ActiveRecord::Migration
  def change
    add_column :projects, :time_zone, :string, :default => "UTC"
  end
end
