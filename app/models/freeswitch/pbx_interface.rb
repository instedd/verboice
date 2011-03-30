module Freeswitch
  class PbxInterface < MagicObjectProtocol::Server
    Port = Rails.configuration.verboice_configuration[:pbx_interface_port].to_i
    DirectoryDir = Rails.configuration.freeswitch_configuration[:directory_dir]

    attr_accessor :pbx

    def call(address, application_id, call_log_id)
      raise "PBX is not available" if pbx.error?
      vars = "{verboice_application_id=#{application_id},verboice_call_log_id=#{call_log_id}}"
      pbx.command "bgapi originate #{vars}#{address} '&socket(localhost:#{Freeswitch::OutboundListener::Port} sync full)'"
      nil
    end

    def update_channel(channel_id)
      channel = Channel.find channel_id
      xml = <<EOF
<include>
  <user id="#{channel.name.parameterize}">
    <gateways>
      <gateway name="#{channel.name.parameterize}">
        <param name="username" value="username"/>
        <param name="password" value="password"/>
        <param name="register" value="false"/>
        <param name="extension" value="#{channel.application_id}"/>
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
  end
end
