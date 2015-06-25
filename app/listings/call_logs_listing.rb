class CallLogsListing < Listings::Base
  include ApplicationHelper

  model do
    # current_account.call_logs
    CallLog.where(account_id: current_account.id).order('call_logs.id DESC')
  end

  paginates_per 10
  layout filters: :top

  filter :id, hidden: true
  filter :direction, select_css_class: 'w10'
  filter :state, select_css_class: 'w10'
  filter :address, title: 'Caller ID', hidden: true
  filter channel: :name, title: 'Channel'
  filter :project_id, hidden: true
  filter project: :name, title: 'Project'

  # filter by after/before

  export :csv, :xls

  column :id, title: 'ID', searchable: true
  column :started_at, title: 'Started' do |_,value|
    render_time value
  end
  column :finished_at, title: 'Finished' do |_,value|
    render_time value
  end
  column 'Duration' do |log|
    distance_of_time_in_words(log.finished_at, log.started_at, true) if log.finished_at
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
  column 'Schedule' do |log|
    log.schedule.try :name
  end

  column project: :name, title: 'Project' do |_,value|
    if format == :html
      listings_link_to_filter value, :project_name, value
    else
      value
    end
  end
  column call_flow: :name, title: 'Call Flow'
  column :state, searchable: true do |log, value|
    if log.fail_reason.present?
      "#{value} (#{log.fail_reason})"
    else
      value
    end
  end
  column '' do |log|
    if format == :html
      details_link = link_to('view details', call_log_path(log), target: '_blank')
      if log.state == :queued
        call = current_account.queued_calls(call_log_id: log.id).first
        delete_link = link_to('', [call.channel, call], :confirm => "Are you sure you want to delete the call #{call.address}?", :method => :delete, :class => "button fdelete")

        delete_link + ' ' + details_link
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
