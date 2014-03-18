class FeedServerController < ApplicationController
  layout false

  expose(:feed) { Feed.find_by_key!(params[:id]) }
  expose(:project) { feed.project }
  expose(:recorded_audios) { project.recorded_audios.order("created_at DESC").limit(50) }

  def get_recording
    respond_to do |format|
      format.wav do
        audio = project.recorded_audios.find(params[:recording_id])
        send_file RecordingManager.for(audio.call_log).result_path_for(audio.key), :x_sendfile => true
      end
    end
  end
end
