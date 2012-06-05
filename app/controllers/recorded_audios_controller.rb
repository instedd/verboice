# Copyright (C) 2010-2012, InSTEDD
# 
# This file is part of Verboice.
# 
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

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
