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

class FeedsController < ApplicationController
  before_filter :authenticate_account!
  expose(:project) { load_project }
  expose(:feeds) { project.feeds }
  expose(:feed)

  before_filter :load_project
  before_filter :check_project_admin, only: [:create, :update, :destroy]

  def index
  end

  def create
    feed.save
    render :partial => "box_content", :locals => { :feed => feed, :expanded => feed.errors.any?}
  end

  def update
    feed.save
    render :partial => "box_content", :locals => { :feed => feed, :expanded => feed.errors.any?}
  end

  def destroy
    feed.destroy
    redirect_to project_feeds_path(project), :notice => "Feed #{feed.name} successfully deleted."
  end
end
