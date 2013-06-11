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

class Commands::NuntiumCommand < Command

  def initialize(resource_guid, rcpt_type, options = {})
    @resource_guid = resource_guid
    @rcpt_type = rcpt_type
    # options should contain
    # - rcpt_address if rcpt_type is '3rdparty'
    # - rcpt_variable if rcpt_type is 'variable'
    # - language to override localization
    @options = options
  end

  def run(session)
    session.info "Send text message '#{@resource_guid}'", command: 'nuntium', action: 'start'

    # determine the address of the recipient of the message
    recipient = rcpt_address(session)

    # extract the text to send from the resource (using localized resource)
    body = localized_resource(session).try(:text)
    body = session.expand_vars(body) if body.present?

    if recipient.blank?
      result = "Missing recipient"
    elsif body.blank?
      result = "Missing text to send"
    else
      # send the message through Nuntium
      begin
        nuntium.send_ao :from => 'sms://verboice', :to => recipient, :body => body, :account_id => session.project.account_id
        result = "OK"
      rescue Pigeon::PigeonError => e
        result = "Nuntium error #{e.message}"
      end
    end

    session.info "Send text message '#{@resource_guid}' finished: #{result}", command: 'nuntium', action: 'finish'
    super
  end

  private

  def nuntium
    Pigeon::Nuntium.from_config
  end

  def rcpt_address(session)
    address = case @rcpt_type
              when 'caller'
                session["var_sms_number"].presence || session.contact.address
              when '3rdparty'
                @options[:rcpt_address]
              when 'variable'
                session["var_#{@options[:rcpt_variable]}"]
              end
    unless address.blank? || address =~ /\A\w+:\/\//
      "sms://#{address}"
    else
      address
    end
  end

  def localized_resource(session)
    language = @options[:language].presence || session.language
    session.project.resources.find_by_guid(@resource_guid).available_resource_for(language)
  end

end
