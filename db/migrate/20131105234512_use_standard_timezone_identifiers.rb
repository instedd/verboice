class UseStandardTimezoneIdentifiers < ActiveRecord::Migration
  def up
    Project.find_each do |project|
      update_time_zone(project)
    end

    QueuedCall.find_each do |queued_call|
      update_time_zone(queued_call)
    end
  end

  def update_time_zone(object)
    if tz = ActiveSupport::TimeZone.find_tzinfo(object.time_zone)
      puts "#{object.time_zone} -> #{tz.identifier}"
      object.time_zone = tz.identifier
      object.save
    else
      puts "Could not find timezone '#{object.time_zone}' for #{object}"
    end
  end

  def down
  end
end
