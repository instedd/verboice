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
  def initialize(options = {})
    @options = {
      :min => self.class.default_minimum_input_lenght,
      :max => self.class.default_maximum_input_lenght,
      :finish_on_key => self.class.default_finish_key,
      :timeout => self.class.default_time_out_in_seconds}
    @options.merge! options
    @options[:max] = Float::INFINITY if @options[:max] < @options[:min]
  end

  def serialize_parameters
    {
      min: @options[:min],
      max: @options[:max],
      finish_on_key: @options[:finish_on_key],
      timeout: @options[:timeout],
    }.tap do |params|
      params[:play] = @options[:play] if @options[:play]
      params[:say] = @options[:say] if @options[:say]
      params[:resource] = @options[:resource] if @options[:resource]
      params[:language] = @options[:language] if @options[:language]
    end
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
