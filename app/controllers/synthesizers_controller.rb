class SynthesizersController < ApplicationController
  def voices
    render json: TTS::Synthesizer.for(params[:engine]).voices
  end
end