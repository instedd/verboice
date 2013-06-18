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
  attr_accessor :start
  attr_accessor :status_callback_url
  attr_accessor :created_at
  attr_accessor :queued_call

  delegate :finish_successfully, :to => :call_log
  CallLogEntry::Levels.each { |severity| delegate severity, :to => :call_log }

  def initialize(options = {})
    @created_at = Time.now
    @vars = {}
    @trace_enabled = true

    options.each do |key, value|
      send "#{key}=", value
    end
  end

  def id
    @id ||= Guid.new.to_s
  end

  def trace_enabled=(value)
    @trace_enabled = value
  end

  def trace_enabled?
    @trace_enabled
  end

  def call_id
    call_log.try(:id)
  end

  def address
    @address || @call_log.address
  end

  def project
    call_log.project
  end

  def contact
    @contact ||= find_or_create_contact
  end

  def find_or_create_contact
    if address.present?
      contact_address = project.contact_addresses.where(address: address).first
      if contact_address.nil?
        contact = project.contacts.new
        contact.addresses.build address: address
        contact.save!
        contact
      else
        contact_address.contact
      end
    else
      contact = project.contacts.new anonymous: true
      contact.addresses.build address: "Anonymous#{call_log.id}"
      contact.save!
      contact
    end
  end

  def broker
    channel.broker.instance
  end

  def js
    @js ||= create_js_context
  end

  def []=(key, value)
    @vars[key] = value
    js[key.to_s] = value
  end

  def [](key)
    value = @vars[key] || js[key.to_s]
    value == :undefined ? nil : value
  end

  def delete(key)
    @vars.delete key
    js[key.to_s] = nil
  end

  def eval(expr)
    result = js.eval expr.to_s
    raise "undefined" if result == :undefined
    result
  end

  def callback_url
    call_flow.callback_url
  end

  def status_callback_url
    @status_callback_url || project.try(:status_callback_url)
  end

  def language
    self['var_language'] || project.default_language
  end

  def voice(lang)
    search = lang || self.language
    match = project.languages.find { |lang| lang['language'] == search }
    match && match['voice'].presence
  end

  def synth(text, options = {})
    voice = voice(options[:language])
    file_id = Digest::MD5.hexdigest "#{text}#{voice}"
    target_path = pbx.sound_path_for file_id
    unless File.exists? target_path
      project.synthesizer.synth text, voice, target_path, options
    end
    target_path
  end

  def expand_vars(string)
    string.gsub(/\{([^\{]*)\}/) do
      self["var_#{$1}"]
    end
  end

  def call_variables=(variables)
    @call_variables = variables
  end

  def run
    raise "Answering machine detected" if call_log.outgoing? && pbx.is_answering_machine?

    load_variables

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
    call_log.update_attributes state: :suspended if call_log
  end

  def resume
    @suspended = false
    call_log.update_attributes state: :active if call_log
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

  if Rails.env == 'production'
    def trace(*)
    end
  end

  def run_command
    raise "Quit" if @quit
    @commands = @commands.run(self)
  end

  def create_js_context
    ctx = RKelly::Runtime.new

    ['digits', 'timeout', 'finish_key'].each { |key| ctx[key] = nil }
    ['answer', 'assign', 'callback', 'capture', 'hangup', 'js', 'play_url', 'pause', 'record', 'say'].each do |func|
      ctx[func] = lambda do |*options|
        if options.length == 1 && options[0].respond_to?(:to_hash)
          options[0] = options[0].to_hash
          options[0].symbolize_keys!
        end
        "Commands::#{func.camelcase}Command".constantize.new(*options).run self
      end
    end
    ctx['alert'] = lambda { |str| info str }
    ctx['record_url'] = lambda { |key| record_url(key) }
    ctx
  end

  def notify_status(status, reason = nil)
    if status_callback_url.present?
      status_callback_url_user = project.status_callback_url_user
      status_callback_url_password = project.status_callback_url_password

      authentication = if (status_callback_url_user.present? || status_callback_url_password.present?)
        {:head => {'authorization' => [status_callback_url_user, status_callback_url_password]}}
      else
        {}
      end

      request = EventMachine::HttpRequest.new status_callback_url
      query = { :CallSid => call_id, :CallStatus => status }
      query[:From] = pbx.caller_id if pbx
      query[:CallDuration] = Time.now - start if start
      query[:Reason] = reason if reason
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

  def load_variables
    unless contact.anonymous?
      contact.persisted_variables.includes(:project_variable).each do |var|
        name = var.implicit_key || var.project_variable.name
        self["var_#{name}"] = var.typecasted_value if var.value.present?
      end
    end
    if @call_variables
      @call_variables.each do |name, value|
        self["var_#{name}"] = value if value.present?
      end
    end
  end
end
