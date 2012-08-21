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

class QueuedCallsController < ApplicationController
  before_filter :authenticate_account!

  def index
    @page = params[:page] || 1
    @per_page = 10
    @channel = current_account.channels.find params[:channel_id]
    @queued_calls = @channel.queued_calls.paginate :page => @page, :per_page => @per_page

    render :layout => false
  end

  def destroy
    @channel = current_account.channels.includes(:queued_calls).find params[:channel_id]
    @call = @channel.queued_calls.find(params[:id])
    @call.cancel_call!
    @call.destroy

    redirect_to(queued_call_logs_path, :notice => "Call #{@call.address} successfully canceled.")
  end
end
