module Freeswitch
  class Broker < BaseBroker
    DirectoryDir = Rails.configuration.freeswitch_configuration[:directory_dir]

    attr_accessor :freeswitch_client

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

    def delete_channel(channel)
      check_freeswitch_available!

      # TODO
    end

    def check_freeswitch_available!
      raise "Freeswitch is not available" if freeswitch_client.error?
    end
  end
end
