class RecordedAudiosController < ApplicationController

  before_filter :authenticate_account!
  before_filter :load_recorded_audio_and_contact, :except => :index

  def index
    @contact = current_account.contacts.includes(:recorded_audios).find(params[:contact_id])
    @recorded_audios = @contact.recorded_audios
  end

  def show
  end

  def destroy
    @recorded_audio.destroy
    redirect_to contact_recorded_audios_path(@contact)
  end

  private

  def load_recorded_audio_and_contact
    @contact = current_account.contacts.includes(:recorded_audios).find(params[:contact_id])
    @recorded_audio = @contact.recorded_audios.find(params[:id])
  end
end
