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

class PermissionsController < ApplicationController
  before_filter :authenticate_account!

  def index
    @projects = current_account.projects.all
    @channels = current_account.channels.all
    @permissions = Permission.includes(:account).
      for_project_ids_and_channel_ids(@projects.map(&:id), @channels.map(&:id))
    @accounts = @permissions.map(&:account).uniq
  end

  def autocomplete
    project_ids = current_account.projects.pluck(:id)
    channel_ids = current_account.channels.pluck(:id)

    used_account_ids = Permission.
      for_project_ids_and_channel_ids(project_ids, channel_ids).
      pluck(:account_id)

    accounts = Account.
      where('email LIKE ?', "#{params[:term]}%").
      where('id != ?', current_account.id)

    unless used_account_ids.empty?
      accounts = accounts.where('id not in (?)', used_account_ids)
    end

    render json: accounts.pluck(:email)
  end

  def add_account
    email = params[:email]
    account = Account.find_by_email email

    unless account
      return render json: {ok: false, error: 'User not found'}
    end

    render json: {ok: true, account: {id: account.id, email: account.email, permissions: []}}
  end

  def update
    account_id = params[:account_id]
    type = params[:type]
    model_id = params[:model_id]
    role = params[:role]

    case type
    when "Project"
      # check that project exists in this account
      current_account.projects.find(model_id)
    when "Channel"
      # check that channel exists in this account
      current_account.channels.find(model_id)
    else
      return head :bad_request
    end

    permission = Permission.where(account_id: account_id, type: type, model_id: model_id).first
    case role
    when "none"
      permission.destroy if permission
    else
      if permission
        permission.role = role
        permission.save!
      else
        Permission.create!(account_id: account_id, type: type, model_id: model_id, role: role)
      end
    end

    head :ok
  end
end
