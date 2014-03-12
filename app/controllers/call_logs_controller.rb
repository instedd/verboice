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

class CallLogsController < ApplicationController
  before_filter :authenticate_account!

  def index
    @page = params[:page] || 1
    @search = params[:search]
    @per_page = 10
    @logs = current_account.call_logs.includes(:project).includes(:channel).order('id DESC')
    @logs = @logs.search @search, :account => current_account if @search.present?
    @logs = @logs.paginate :page => @page, :per_page => @per_page
  end

  def show
    set_fixed_width_content
    @log = current_account.call_logs.find params[:id]
    @activities = Hercule::Activity.search({size: 1000, filter: {
      and: [
        {term: {call_log_id: params[:id]}},
        {exists: {field: "step_type"}}
      ]
    }}).items.sort_by(&:start)
  end

  def progress
    @log = current_account.call_logs.find params[:id]
    render :layout => false
  end

  def queued
    @page = params[:page] || 1
    @per_page = 10
    @calls = current_account.queued_calls.includes(:channel).includes(:call_log).includes(:schedule).order('id DESC')
    @calls = @calls.paginate :page => @page, :per_page => @per_page
  end

  def play_result
    @log = current_account.call_logs.find params[:id]
    send_file RecordingManager.for(@log).result_path_for(params[:key]), :x_sendfile => true
  end

  def download
    @filename = "Call_logs_(#{Time.now.to_s.gsub(' ', '_')}).csv"
    @streaming = true
    @csv_options = { :col_sep => ',' }
  end

  def download_details
    @log = current_account.call_logs.includes(:entries).find params[:id]
    @filename = "Call details #{@log.id} (#{Time.now}).csv"
    @streaming = true
    @csv_options = { :col_sep => ',' }
  end

end
