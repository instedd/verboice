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

class ApiLogsController < ApplicationController
  before_filter :authenticate_account!
  skip_before_filter :verify_authenticity_token

  respond_to :csv

  def list
    @log = current_account.call_logs.find_by_id params[:call_id]

    if @log.present?
      @entries = @log.entries
      @entries = @entries.where('id > ?', params[:after]) if params[:after].present?
    else
      head :not_found
      return
    end
  end

end
