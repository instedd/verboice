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

class CallLogsListing < Listings::Base
  include ApplicationHelper

  class << self
    attr_accessor :test_account
  end

  model do
    CallLog.for_account(listing_account).order('call_logs.id DESC')
  end

  def listing_account
    self.class.test_account || current_account
  end

  paginates_per 10
  layout filters: :top

  filter :id, render: false
  filter :direction, select_css_class: 'w10'
  filter :state, select_css_class: 'w10'
  filter :address, title: 'Caller ID', render: false
  filter :channel_id, render: false
  filter channel: :name, title: 'Channel'
  filter :project_id, render: false
  filter project: :name, title: 'Project'

  custom_filter :after do |items, value|
    items.where "started_at >= ?", Time.smart_parse(value)
  end

  custom_filter :before do |items, value|
    items.where "started_at <= ?", Time.smart_parse(value)
  end

  export :csv, :xls

  column :id, title: 'ID', searchable: true
  column :started_at, title: 'Started' do |_,value|
    render_time value
  end
  column 'Duration' do |log|
    distance_of_time_in_words(log.finished_at, log.started_at, true) if log.finished_at && log.started_at
  end
  column :address, title: 'Caller ID', searchable: true do |_,value|
    if value.present? && format == :html
      listings_link_to_filter value, :address, value
    else
      value
    end
  end
  column :direction, searchable: true
  column channel: :name, title: 'Channel'
  column schedule: :name, title: 'Schedule'
  column project: :name, title: 'Project' do |_,value|
    if format == :html
      listings_link_to_filter value, :project_name, value
    else
      value
    end
  end
  column call_flow: :name, title: 'Call Flow'
  column :state, searchable: true do |log, value|
    text = if log.fail_reason.present?
      "#{value.capitalize} (#{log.fail_reason})"
    else
      value.capitalize
    end
    if format == :html
      content_tag(:div, listings_link_to_filter(text, :state, value.to_s), title: log.fail_details)
    else
      text
    end
  end
  column :fail_details, title: "Failure" do |log, value|
    value if log.state == :failed
  end
  column '' do |log|
    if format == :html
      details_link = link_to('View details', call_log_path(log))
      if log.state == :queued
        call = listing_account.queued_calls.where(call_log_id: log.id).first
        if call
          delete_link = link_to('', [call.channel, call], :confirm => "Are you sure you want to delete the call #{log.id} to #{call.address}?", :method => :delete, :class => "button fdelete")

          delete_link + ' ' + details_link
        else
          details_link
        end
      else
        details_link
      end
    end
  end

  def render_time(value)
    if format == :html
      raw time_ago value
    else
      value
    end
  end
end
