module Freeswitch
  class PbxInterface < MagicObjectProtocol::Server
    Port = Rails.configuration.verboice_configuration[:pbx_interface_port].to_i

    attr_accessor :pbx

    def call(address, application_id, call_log_id)
      raise "PBX is not available" if pbx.error?
      vars = "{verboice_application_id=#{application_id},verboice_call_log_id=#{call_log_id}}"
      pbx.command "bgapi originate #{vars}#{address} '&socket(localhost:#{Freeswitch::OutboundListener::Port} sync full)'"
      nil
    end
  end
end
