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
  def self.start
    Asterisk::Broker.instance.start
    Twilio::Broker.instance.start
    Voxeo::Broker.instance.start

    EM::start_server 'localhost', BrokerFacade::PORT, BrokerFacade

    EM.add_periodic_timer(20) do
      begin
        Fiber.new { queued_calls.each &:notify_broker }.resume
      rescue Exception => ex
        puts ex
      end
    end
  end

  def self.queued_calls
    QueuedCall.where('not_before IS NULL OR not_before <= ?', Time.now.utc).order(:not_before).select([:id, :channel_id]).includes(:channel)
  end

  def wake_up_queued_calls
    channels.joins(:queued_calls).each do |channel|
      notify_call_queued channel
    end
  end

  def notify_call_queued channel
    log "Notified call queued for channel #{channel.id}"
    log "PBX is unavailable" and return unless pbx_available?
    log "Reached active calls limit for channel #{channel.id}" and return if reached_active_calls_limit?(channel)

    queued_call = channel.poll_call
    log "No queued calls for channel #{channel.id}" and return unless queued_call

    if queued_call.session_id
      session = find_session(queued_call.session_id)
      log "Resuming call #{session.call_id} from queued call #{queued_call.id} on channel #{channel.id}"
    else
      session = queued_call.start
      store_session session
      log "Starting new call #{session.call_id} from queued call #{queued_call.id} on channel #{channel.id}"
    end

    begin
      call session
      log "Call #{session.call_id} is ringing" and session.notify_status 'ringing'
    rescue PbxUnavailableException => ex
      log "Pbx is unavailable for call #{session.call_id}: #{ex.message}\n#{ex.backtrace.join("\n")}"
      finish_session_with_requeue session, error_message_for(ex), queued_call
    rescue Exception => ex
      log "Error performing call #{session.call_id}: #{ex.message}\n#{ex.backtrace.join("\n")}"
      finish_session_with_error session, error_message_for(ex)
    end
  end

  def reached_active_calls_limit?(channel)
    channel.has_limit? && active_calls_count_for(channel) >= channel.limit.to_i
  end

  def active_calls
    @active_calls ||= Hash.new {|hash, key| hash[key] = {}}
  end

  def active_calls_count_for(channel)
    active_calls[channel.id].count { |session_id, session| !session.suspended }
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
    session.start = Time.now
    log "Accepting call from PBX with call id #{session.call_id}"
    log "Session for call #{session.call_id} is suspended" and return session.resume if session.suspended
    session.call_log.address = pbx.caller_id unless session.call_log.address.present?
    begin
      log "Call #{session.call_id} is now in progress" and session.notify_status 'in-progress'
      session.run
    rescue Exception => ex
      log "Call #{session.call_id} failed with exception: #{ex.message}\n#{ex.backtrace}"
      handle_failed_call session, ex.message, :failed
    else
      finish_session_without_error session
    ensure
      session.pbx.hangup rescue nil
      EM.fiber_sleep 2
      notify_call_queued session.channel
    end
  end

  def find_or_create_session(pbx)
    session = find_session pbx.session_id
    unless session
      channel = find_channel(pbx)
      session = channel.new_session
      store_session session
    end
    session.pbx = pbx
    pbx.session = session
    session
  end

  def find_channel(pbx)
    Channel.find(pbx.channel_id)
  end

  def find_session(id)
    sessions[id]
  end

  def find_session_by_call_log_id(id)
    sessions.values.select {|x| x.call_log.id == id}.first
  end

  def finish_session_without_error(session)
    if session['status'] == 'failed'
      handle_failed_call session, "Call was marked as failed", :failed
    else
      finish_session_successfully(session)
    end
  end

  def finish_session_successfully(session)
    log "Call #{session.call_id} finished successfully"
    session = find_session session unless session.is_a? Session
    session.notify_status 'completed'
    session.finish_successfully
  rescue Exception => ex
    log "Error when completing session #{session.id} successfully: #{ex}"
  ensure
    finish_session session
  end

  def finish_session_with_error(session, error_message, status = 'failed')
    session = find_session session unless session.is_a? Session
    session.notify_status status
    session.finish_with_error error_message
  rescue Exception => ex
    log "Error when completing session #{session.id} with error: #{ex}"
  ensure
    finish_session session
  end

  def finish_session_with_requeue(session, error_message, queued_call)
    queued_call.call_log.warn error_message
    queued_call.call_log.fail_reason = error_message
    queued_call.call_log.state = :queued
    queued_call.call_log.save!

    queued_call.dup.save!
  rescue Exception => ex
    log "Failed to requeue call #{queued_call.id}: #{ex}"
  ensure
    finish_session session
  end

  def call_rejected(session_id, reason)
    session = find_session session_id
    message = case reason
    when :busy then 'Remote end is busy'
    when :no_answer then 'Remote end do not answer'
    else 'Failed to establish the communication'
    end

    log "Call for session #{session_id} rejected: #{reason}"

    handle_failed_call session, message, reason

    EM.fiber_sleep 2
    notify_call_queued session.channel
  end

  def associate_pbx_log session_id, pbx_log_id
    session = find_session session_id
    session.call_log.update_attributes :pbx_logs_guid => pbx_log_id
  end

  def handle_failed_call(session, message, reason)
    if session['status'] == 'successful'
      finish_session_successfully(session)
      return
    end

    queued_call = session.queued_call
    next_address = session.contact.next_address(session.address)
    if queued_call && (queued_call.has_retries_left? || next_address.present?)
      queued_call = queued_call.dup
      if next_address.present?
        queued_call.address = next_address
        queued_call.not_before = 15.seconds.from_now
      else
        queued_call.address = session.contact.first_address
        queued_call.retries += 1
        queued_call.not_before = queued_call.next_retry_time
      end

      log "Re enqueuing call for session #{session.id} with queued call #{queued_call.id} for #{queued_call.not_before}"
      session.notify_status :queued, reason.to_s.dasherize
      finish_session_with_requeue session, message, queued_call
    else
      log "Dropping call for session #{session.id}"
      finish_session_with_error session, message, reason.to_s.dasherize
    end
  end

  def channels
    subclass_responsibility
  end

  private

  def store_session(session)
    sessions[session.id] = session
    active_calls[session.channel.id][session.id] = session
  end

  def finish_session(session)
    sessions.delete session.id
    active_calls[session.channel.id].delete session.id
    active_calls.delete session.channel.id if active_calls[session.channel.id].empty?
  end

  def error_message_for(exception)
    if Rails.env.production?
      exception.message
    else
      "Exception #{exception.class}: #{exception.message}\n#{exception.backtrace.join("\n")}"
    end
  end

  def log(str)
    Rails.logger.info(str)
    true
  end
end
