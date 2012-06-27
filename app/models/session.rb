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

class Session
  attr_accessor :pbx
  attr_accessor :commands
  attr_accessor :call_flow
  attr_accessor :channel
  attr_accessor :call_log
  attr_accessor :address
  attr_accessor :suspended

  delegate :finish_successfully, :to => :call_log
  CallLogEntry::Levels.each { |severity| delegate severity, :to => :call_log }

  def initialize(options = {})
    @vars = {}
    @log_level = :trace

    options.each do |key, value|
      send "#{key}=", value
    end
  end

  def id
    @id ||= Guid.new.to_s
  end

  def call_id
    call_log.try(:id)
  end

  def js
    @js ||= new_v8_context
  end

  def []=(key, value)
    @vars[key] = value
    js[key.to_s] = value
  end

  def [](key)
    @vars[key]
  end

  def delete(key)
    @vars.delete key
    js[key.to_s] = nil
  end

  def eval(expr)
    js.eval expr.to_s
  end

  def callback_url
    call_flow.callback_url
  end

  def status_callback_url
    call_flow.project.try(:status_callback_url)
  end

  def run
    raise "Answering machine detected" if call_log.outgoing? && pbx.is_answering_machine?

    loop do
      begin
        run_command until commands.nil? || @suspended
      rescue Exception => ex
        raise ex unless @suspended
      end

      break unless @suspended
      @suspend_fiber = Fiber.current
      Fiber.yield
    end
  end

  def suspend
    @suspended = true
  end

  def resume
    @suspended = false
    @suspend_fiber.resume
  end

  def quit!
    @quit = true
  end

  CallLogEntry::Levels.each do |name|
    class_eval %Q(
      def #{name}(text, options ={})
        call_log.#{name} text, step_id: self['current_step'], step_name: self['current_step_name'], command: options[:command], action: options[:action]
      end
    )
  end

  def log(options)
    if @log_level == :trace
      call_log.trace options[:trace]
    else
      call_log.info options[:info]
    end
  end

  def run_command
    raise "Quit" if @quit
    @commands = @commands.run(self)
  end

  def new_v8_context
    ctx = V8::Context.new
    ['digits', 'timeout', 'finish_key'].each { |key| ctx[key] = nil }
    ['answer', 'assign', 'callback', 'capture', 'hangup', 'js', 'play', 'pause', 'record', 'say'].each do |func|
      ctx[func] = lambda do |*options|
        options.shift # First argument is always Javascript's 'this'
        if options.length == 1 && options[0].respond_to?(:to_hash)
          options[0] = options[0].to_hash
          options[0].symbolize_keys!
        end
        "Commands::#{func.camelcase}Command".constantize.new(*options).run self
      end
    end
    ctx['alert'] = lambda { |this, str| info str }
    ctx['record_url'] = lambda { |this, key| record_url(key) }
    ctx
  end

  def notify_status(status)
    if status_callback_url.present?
      status_callback_url_user = call_flow.project.status_callback_url_user
      status_callback_url_password = call_flow.project.status_callback_url_password

      authentication = if (status_callback_url_user.present? || status_callback_url_password.present?)
        {:head => {'authorization' => [status_callback_url_user, status_callback_url_password]}}
      else
        {}
      end

      request = EventMachine::HttpRequest.new status_callback_url
      query = { :CallSid => call_id, :CallStatus => status }
      query[:From] = pbx.caller_id if pbx
      request.get({:query => query}.merge(authentication))
    end
  end

  def finish_with_error message
    @commands = call_flow.error_flow
    run rescue nil

    error message
    call_log.finish_with_error message
  end

  def record_url(key)
    NamedRoutes.result_call_log_url(call_log, :key => key.to_i)
  end

  def recording_manager
    @recording_manager ||= RecordingManager.for(call_flow)
  end
end
