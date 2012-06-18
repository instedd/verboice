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

module Voxeo
  class CallManager

    attr_reader :session_id, :voxeo_session_id, :channel_id, :caller_id

    def initialize channel_id, voxeo_session_id, opts = {}
      @channel_id = channel_id
      @voxeo_session_id = voxeo_session_id
      @session_id = opts[:session_id]
      @caller_id = opts[:caller_id]
      @context = opts[:context]
      @builder = Builders::Vxml.new callback_url, "sessionid" => voxeo_session_id
      @hangup = false
      @config = Rails.configuration.voxeo_configuration
    end

    def answer
    end

    def play(filename, escape_digits = nil)
      return if @hangup
      @builder.play sounds_url_for(filename)
    end

    def say(text, opts = {})
      return if @hangup
      @builder.say text
    end

    def pause(length)
      return if @hangup
      @builder.pause length
    end

    def capture(options)
      return if @hangup

      options[:play] = sounds_url_for(options[:play]) if options[:play]

      @builder.capture options
      @builder.callback callback_url

      flush
      @context.params[:digits]
    end

    def hangup
      return if @hangup

      @builder.hangup

      end_session
    end

    def bridge_with(other_session)
      # TODO
    end

    def dial(address, options = {})
      # TODO
    end

    def is_answering_machine?
      false
    end

    def sound_path_for(basename)
      File.join sounds_path, "#{basename}.gsm"
    end

    private

    def flush
      begin
        @context = Fiber.yield @builder.build
      rescue Exception => e
        handle_error e
      end
    end

    def handle_error(e)
      @hangup = true

      @builder.say "An unexpected error ocurred"
      @builder.hangup

      # End the session from the store
      Voxeo::SessionStore.instance.session_for(@voxeo_session_id).end!

      # Enqueue operation to resume the fiber so the session can end
      current_fiber = Fiber.current
      EM.next_tick { current_fiber.resume e }

      Fiber.yield @builder.build
    end

    def end_session
      @hangup = true

      # End the session from the store
      Voxeo::SessionStore.instance.session_for(@voxeo_session_id).end!

      # Enqueue operation to resume the fiber so the session can end
      current_fiber = Fiber.current
      EM.next_tick { current_fiber.resume }

      flush
    end

    def sounds_path
      File.join(Rails.root, 'data', 'voxeo')
    end

    def callback_url
      Voxeo::UrlHelper.callback_url :host => @context.headers[:Host]
    end

    def sounds_url_for(filename)
      key = Guid.new.to_s
      Voxeo::SessionStore.instance.session_for(@voxeo_session_id).store(key, filename)
      Voxeo::UrlHelper.audio_url key, :sessionid => @voxeo_session_id, :host => @context.headers[:Host]
    end

  end
end