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

module Freeswitch
  class Broker < BaseBroker
    DirectoryDir = Rails.configuration.freeswitch_configuration[:directory_dir]

    attr_accessor :freeswitch_client

    def self.instance
      $freeswitch_broker ||= new
    end

    def call(queued_call)
      check_freeswitch_available!

      vars = "{verboice_channel_id=#{queued_call.channel.id},verboice_call_log_id=#{queued_call.call_log.id}}"
      freeswitch_client.command "bgapi originate #{vars}#{queued_call.address} '&socket(localhost:#{Freeswitch::CallManager::Port} sync full)'"
      nil
    end

    def create_channel(channel)
      check_freeswitch_available!

      xml = <<EOF
<include>
  <user id="#{channel.name.parameterize}">
    <gateways>
      <gateway name="#{channel.name.parameterize}">
        <param name="username" value="username"/>
        <param name="password" value="password"/>
        <param name="register" value="false"/>
        <param name="extension" value="#{channel.project_id}"/>
        <param name="context" value="verboice"/>
      </gateway>
    </gateways>
    <params>
      <param name="password" value="password"/>
    </params>
    <variables>
      <variable name="verboice_account_id" value="#{channel.account_id}" />
      <variable name="user_context" value="verboice" />
    </variables>
  </user>
</include>
EOF
      path = File.join DirectoryDir, "verboice_#{channel_id}.xml"
      File.open(path, 'w') do |file|
        file.write xml
      end
    end

    def destroy_channel(channel)
      check_freeswitch_available!

      # TODO
    end

    def check_freeswitch_available!
      raise "Freeswitch is not available" if freeswitch_client.error?
    end

    def channels
      Channel.where("type != '#{Channels::Voxeo.name}'")
    end
  end
end
