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

class Commands::CaptureCommand < Command
  param :min, :integer, :default => 1, :ui_length => 1
  param :max, :integer, :default => 1, :ui_length => 1
  param :finish_on_key, :string, :default => '#', :ui_length => 1
  param :timeout, :integer, :default => 5, :ui_length => 1
  param :play, :string, :ui_length => 40
  param :say, :string, :ui_length => 40

  def initialize(options = {})
    @options = {
      :min => self.class.default_minimum_input_lenght,
      :max => self.class.default_maximum_input_lenght,
      :finish_on_key => self.class.default_finish_key,
      :timeout => self.class.default_time_out_in_seconds}
    @options.merge! options
    @options[:max] = Float::INFINITY if @options[:max] < @options[:min]
  end

  def run(session)

    options = @options.dup
    if options[:play].present?
      options[:play] = Commands::PlayUrlCommand.new(options[:play]).download(session)
      options.delete :say
    elsif options[:play_file].present?
      options[:play] = Commands::PlayFileCommand.new(options[:play_file]).download(session)
      options.delete :say
    elsif options[:say].present?
      options.delete :play
    else
      options.delete :play
      options.delete :say
    end

    [:digits, :timeout, :finish_key].each { |key| session.delete key }

    options[:after_play] = lambda() do |digits, offset|
      if digits
        session.info "User interrupted playback at #{offset} milliseconds."
      else
        session.info "Finished playing file."
        session.info "Waiting for user input."
      end
    end

    if options[:say].present?
      session.log(
        :info => "Say '#{@options[:say]}'. Waiting user input",
        :trace => "Say '#{@options[:say]}'. Waiting user input: #{@options.to_pretty_s}"
      )
    elsif options[:play].present?
      session.log(
        :info => "Play file #{@options[:play]}. Waiting user input",
        :trace => "Play file #{@options[:play]}. Waiting user input: #{@options.to_pretty_s}"
      )
    else
      session.log(
        :info => "Waiting user input",
        :trace => "Waiting user input: #{@options.to_pretty_s}"
      )
    end

    digits = session.pbx.capture options
    case digits
    when nil
      session.info("User didn't press enough digits")
      session[:timeout] = true
    when :timeout
      session.info("User timeout")
      session[:timeout] = true
    when :finish_key
      session.info("User pressed the finish key")
      session[:finish_key] = true
    else
      session.info("User pressed: #{digits}")
      session[:digits] = digits
    end

    super
  end

  def self.default_time_out_in_seconds
    5
  end

  def self.default_minimum_input_lenght
    1
  end

  def self.default_maximum_input_lenght
    1
  end

  def self.default_finish_key
    '#'
  end
end
