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

class ProjectsController < ApplicationController
  before_filter :authenticate_account!
  before_filter :load_project, only: [:edit, :update, :update_variables]
  before_filter :load_enqueue_call_fields, only: [:show, :enqueue_call]
  before_filter :check_project_admin, only: [:update, :update_variables]

  def index
    @projects = current_account.projects.all
    @shared_projects = current_account.shared_projects.all
    @all_projects = @projects.map { |p| [p, nil] } +
                    @shared_projects.map { |sp| [sp.project, sp.role] }
  end

  def show
  end

  def new
    @project = Project.new
  end

  def edit
  end

  def create
    @project = Project.new(params[:project])
    @project.account = current_account

    if @project.save
      redirect_to(project_call_flows_path(@project), :notice => "Project #{@project.name} successfully created.")
    else
      render :action => "new"
    end
  end

  def update
    if @project.update_attributes(params[:project])
      redirect_to(project_path(@project), :notice => "Project #{@project.name} successfully updated.")
    else
      render :action => "edit"
    end
  end

  def enqueue_call
    redirect_to project_path(params[:id]), flash: {error: 'You need to select a Call Flow'} and return unless params[:call_flow_id].present?

    @channel = @channels.find { |c| c.id == params[:channel_id].to_i }
    redirect_to project_path(params[:id]), flash: {error: 'You need to select a channel'} and return unless @channel
    redirect_to project_path(params[:id]), flash: {error: 'The channel is disabled'} and return unless @channel.enabled?

    addresses = params[:addresses].split(/\n/).map(&:strip).select(&:presence)

    options = {}
    options[:account] = current_account
    options[:schedule_id] = params[:schedule_id] if params[:schedule_id].present?
    options[:not_before] = "#{params[:not_before_date]} #{params[:not_before_time]}" if params[:not_before_date].present? && params[:not_before].present?
    options[:not_after] = "#{params[:not_after_date]} #{params[:not_after_time]}" if params[:not_after_date].present? && params[:not_after].present?
    options[:time_zone] = params[:time_zone] if params[:time_zone].present?
    options[:call_flow_id] = params[:call_flow_id] if params[:call_flow_id].present?
    options[:project_id] = params[:id]
    options[:vars] = params[:vars]

    addresses = curated_addresses(addresses)
    addresses.each do |address|
      @channel.call(address.strip, options)
    end

    redirect_to project_path(params[:id]), :notice => "Enqueued calls to #{pluralize(addresses.count, 'address')} on channel #{@channel.name}"
  rescue CallQueuingError => e
    redirect_to project_path(params[:id]), flash: {error: e.message}
  end

  def destroy
    @project = current_account.projects.find(params[:id])
    @project.destroy
    redirect_to(projects_url, :notice => "Project #{@project.name} successfully deleted.")
  end

  def update_variables
    if @project.update_attributes(params[:project])
      redirect_to project_contacts_path(@project), notice: "Columns successfully updated."
    else
      redirect_to project_contacts_path(@project), flash: { error: "Error updating columns."}
    end
  end

  private

  def load_enqueue_call_fields
    load_project
    @channels = current_account.channels.all

    shared_channels = current_account.shared_channels.all.map(&:channel)
    shared_channels.each { |c| c.name = "#{c.name} (shared)" }
    @channels.concat shared_channels

    @schedules = @project.schedules
    @call_flows = @project.call_flows.includes(:channels).includes(:queued_calls)
    @project_channels = @call_flows.collect(&:channels).flatten.to_set
    @queued_calls = @call_flows.collect(&:queued_calls).flatten
    @call_logs = @project.call_logs
  end

  def curated_addresses(addresses)
    # build a hash from contact_id to all his addresses
    # eg. { 1 => ['123','456'], 2 => ['789'] }
    all_contacts = Hash.new { |hash,key| hash[key] = [] }
    addresses_contacts = @project.contact_addresses.where(address: addresses).pluck(:contact_id)
    all_contacts = @project.contact_addresses.where(contact_id: addresses_contacts).order(:id).inject(all_contacts) do |contacts, contact_address|
      contacts[contact_address.contact_id] << contact_address.address
      contacts
    end
    # now build a hash from every contact's addresses to all his addresses
    # eg. { '123' => ['123', '456'], '456' => ['123', '456'], '789' => ['789'] }
    all_addresses = all_contacts.values.inject({}) do |all_addresses, contact_addresses|
      contact_addresses.each do |contact_address|
        all_addresses[contact_address] = contact_addresses
      end
      all_addresses
    end

    # curate the addresses so that there is only one entry for each contact in
    # the original list, and that entry is the contact's first registered address
    known_addresses = Set.new
    addresses.inject([]) do |curated, address|
      unless known_addresses.include?(address)
        contact_addresses = all_addresses[address]
        if contact_addresses.nil?
          known_addresses << address
          curated << address
        else
          known_addresses.merge(contact_addresses)
          curated << contact_addresses.first
        end
      end
      curated
    end
  end
end
