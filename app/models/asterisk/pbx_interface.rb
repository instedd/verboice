module Asterisk
  class PbxInterface < MagicObjectProtocol::Server
    attr_accessor :pbx

    def call(address, application_id, call_log_id)
      raise "PBX is not available" if pbx.error?
      result = pbx.originate :channel => address,
        :application => 'AGI',
        :data => "agi://localhost:#{FastAgiPort},#{application_id},#{call_log_id}",
      :async => true,
        :actionid => call_log_id
      raise result[:message] if result[:response] == 'Error'
      nil
    end
  end
end
