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
module Api
  class CallsController < ApiController

    def call
      begin
        params[:flow] = request.body.read if request.post?
        call_log = current_account.call params
        render :json => {:call_id => call_log.id, :state => call_log.state}
      rescue Exception => ex
        render :status => 400, :json => {error: ex.message}
      end
    end

    def redirect
      options = {}
      if request.post?
        options[:flow] = Parsers::Xml.parse request.body
      elsif params[:call_flow_id]
        if not current_account.call_flows.exists? params[:call_flow_id]
          return render :status => 404
        end
        options[:call_flow_id] = params[:call_flow_id]
      elsif params[:project_id]
        if not current_account.projects.exists? params[:project_id]
          return render :status => 404
        end
        options[:project_id] = params[:project_id]
      elsif params[:callback_url]
        options[:callback_url] = params[:callback_url]
      else
        return render :status => 400
      end

      channel = CallLog.find(params[:id]).channel
      BrokerClient.redirect options

      render :text => 'OK'
    end

    def state
      call_log = current_account.call_logs.where(:id => params[:id]).first
      render :json => {:call_id => call_log.id, :state => call_log.state}
    end

    def details
      call_log = current_account.call_logs.where(:id => params[:id]).first
      return head :not_found if call_log.nil?

      render :json => {
        :call_id => call_log.id,
        :state => call_log.state,
        :project_id => call_log.project.id,
        :project_name => call_log.project.try(:name),
        :started_at => call_log.started_at,
        :finished_at => call_log.finished_at,
        :not_before => call_log.not_before,
        :direction => call_log.direction,
        :channel_id => call_log.channel.id,
        :channel_name => call_log.channel.try(:name),
        :call_flow_id => call_log.call_flow.id,
        :call_flow_name => call_log.call_flow.try(:name),
        :contact_id => call_log.contact_id,
        :contact_address => call_log.address,
        :schedule_id => call_log.schedule_id,
        :schedule_name => call_log.schedule.try(:name),
        :fail_reason => call_log.fail_reason,
        :fail_code => call_log.fail_code,
        :fail_details => call_log.fail_details
      }.reject {|k,v| v.nil?}
    end

    def cancel
      call_log = current_account.call_logs.where(:id => params[:id]).first
      return head :not_found if call_log.nil?

      queued_calls = QueuedCall.where(call_log_id: call_log.id).all
      queued_calls.each do |qc|
        qc.cancel_call!
        qc.destroy
      end

      call_log.state = "canceled"
      call_log.save!

      render :json => {
        :call_id => call_log.id,
        :state => call_log.state,
      }
    end
  end
end
