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

class Commands::RecordCommand < Command

  attr_accessor :filename, :stop_keys, :timeout

  def initialize key, description, options = {}
    @key         = key
    @description = description
    @stop_keys   = options[:stop_keys] || '01234567890*#'
    @timeout     = options[:timeout].try(:to_i) || 10
  end

  def run(session)
    session.info "Record user voice"
    session.pbx.record filename(session), stop_keys, timeout
    create_recorded_audio(session)
    super
  end

  private

  def filename(session)
    RecordingManager.for(session.call_log).result_path_for(@key)
  end

  def create_recorded_audio(session)
    account = session.call_log.account

    contact = if session.address.presence
      account.contacts.where(:address => session.address).first_or_create!
    else
      account.contacts.where(:address => "Anonymous#{session.call_log.id}", :anonymous => true).first_or_create!
    end
    session.trace "Caller address is unknown. Recording '#{@description}' saved for contact #{contact.address}." unless session.address.presence

    contact.recorded_audios.create! :call_log => session.call_log, :key => @key, :description => @description
  end
end
