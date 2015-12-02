module Telemetry::ProjectTracking

  extend ActiveSupport::Concern

  included do
    after_save    :telemetry_track_activity
    after_destroy :telemetry_track_activity
  end

  def telemetry_track_activity
    project.telemetry_track_activity if project.present?
  end

end