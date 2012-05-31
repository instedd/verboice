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

class BaseBroker
  class << self
    attr_accessor :instance
  end

  def wake_up_queued_calls
    Channel.joins(:queued_calls).each do |channel|
      notify_call_queued channel
    end
  end

  def notify_call_queued channel
    return unless pbx_available?
    return if reached_active_calls_limit?(channel)

    queued_call = channel.poll_call
    return unless queued_call

    session = queued_call.start
    store_session session, queued_call

    begin
      call session
      session.notify_status 'ringing'
    rescue PbxUnavailableException => ex
      finish_session_with_requeue session, ex.message, queued_call
    rescue Exception => ex
      finish_session_with_error session, ex.message
    end
  end

  def schedule_call not_before
    if @scheduled_at.nil? || @scheduled_at > not_before
      @scheduled_at = not_before
      EM.cancel_timer(@scheduled_timer) if @scheduled_timer
      @scheduled_timer = EM.add_timer(not_before - Time.now) do
        Fiber.new do
          @scheduled_at = nil
          @scheduled_timer = nil

          QueuedCall.where('not_before <= ?', not_before).order(:not_before).select(:channel_id).includes(:channel).each do |queued_call|
            notify_call_queued queued_call.channel
          end

          next_call = QueuedCall.where('not_before > ?', not_before).order(:not_before).first
          schedule_call next_call.not_before if next_call
        end.resume
      end
    end
  end

  def reached_active_calls_limit?(channel)
    channel.has_limit? && active_calls_count_for(channel) >= channel.limit.to_i
  end

  def active_calls
    @active_calls ||= Hash.new {|hash, key| hash[key] = {}}
  end

  def active_queued_calls
    @active_queued_calls ||= {}
  end

  def active_calls_count_for(channel)
    active_calls[channel.id].length
  end

  def redirect(session_id, options = {})
    session = find_session_by_call_log_id session_id.to_i
    if session

      call_flow = if options[:call_flow_id]
        CallFlow.find options[:call_flow_id]
      else
        CallFlow.new options
      end

      session.commands = call_flow.commands
      session.suspend
      restart session
      session.pbx.close_connection
    end
  end

  def sessions
    @sessions ||= {}
  end

  def accept_call(pbx)
    session = find_or_create_session pbx
    return session.resume if session.suspended
    session.call_log.address = pbx.caller_id unless session.call_log.address.present?
    begin
      session.notify_status 'in-progress'
      session.run
    rescue Exception => ex
      finish_session_with_error session, ex.message
    else
      finish_session_successfully session
    ensure
      session.pbx.hangup

      EM.fiber_sleep 2

      notify_call_queued session.channel
    end
  end

  def find_or_create_session(pbx)
    session = find_session pbx.session_id
    unless session
      session = Channel.find(pbx.channel_id).new_session
      store_session session
    end
    session.pbx = pbx
    session
  end

  def find_session(id)
    sessions[id]
  end

  def find_session_by_call_log_id(id)
    sessions.values.select {|x| x.call_log.id == id}.first
  end

  def finish_session_successfully(session)
    session = find_session session unless session.is_a? Session
    session.notify_status 'completed'
    session.finish_successfully

    finish_session session
  end

  def finish_session_with_error(session, error_message, status = 'failed')
    session = find_session session unless session.is_a? Session
    session.notify_status status
    session.finish_with_error error_message

    finish_session session
  end

  def finish_session_with_requeue(session, error_message, queued_call)
    queued_call.call_log.warn error_message
    queued_call.call_log.state = :queued
    queued_call.call_log.save!

    queued_call.dup.save!

    finish_session session
  end

  def call_rejected(session_id, reason)
    session = find_session session_id
    message = case reason
    when :busy then 'Remote end is busy'
    when :no_answer then 'Remote end do not answer'
    else 'Failed to establish the communication'
    end

    queued_call = active_queued_calls[session_id]
    if queued_call && queued_call.schedule && queued_call.schedule.retry_delays.count > queued_call.retries
      queued_call = queued_call.dup
      queued_call.retries += 1
      sleep = queued_call.schedule.retry_delays[queued_call.retries - 1].to_f * (Rails.env == 'development' ? 1.second : 1.hour)
      queued_call.not_before = queued_call.schedule.next_available_time(Time.now.utc + sleep)

      finish_session_with_requeue session, message, queued_call
      schedule_call queued_call.not_before
    else
      finish_session_with_error session, message, reason.to_s.dasherize
    end

    EM.fiber_sleep 2
    notify_call_queued session.channel
  end

  private

  def store_session(session, queued_call = nil)
    sessions[session.id] = session
    active_calls[session.channel.id][session.id] = session
    if queued_call
      active_queued_calls[session.id] = queued_call
    end
  end

  def finish_session(session)
    sessions.delete session.id
    active_calls[session.channel.id].delete session.id
    active_calls.delete session.channel.id if active_calls[session.channel.id].empty?
    active_queued_calls.delete session.id
  end
end
