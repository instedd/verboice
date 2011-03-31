module Asterisk
  class PbxInterface < MagicObjectProtocol::Server
    Port = Rails.configuration.verboice_configuration[:pbx_interface_port].to_i
    ConfigDir = Rails.configuration.asterisk_configuration[:config_dir]
    SipConf = "#{ConfigDir}/sip.conf"

    attr_accessor :pbx

    def call(address, application_id, call_log_id)
      raise "PBX is not available" if pbx.error?
      result = pbx.originate :channel => address,
        :application => 'AGI',
        :data => "agi://localhost:#{Asterisk::FastAGIServer::Port},#{application_id},#{call_log_id}",
        :async => true,
        :actionid => call_log_id
      raise result[:message] if result[:response] == 'Error'
      nil
    end

    def update_channel(channel_id)
      puts "UPDATE #{channel_id}"
      channel = Channel.find channel_id
      options = { :type => :friend, :secret => channel.config['password'], :context => :verboice, :host => :dynamic }
      if channel.host_and_port?
        host, port = channel.host_and_port
        options[:host] = host
        options[:port] = port if port
      end
      Asterisk::Conf.change SipConf do
        add "verboice_#{channel_id}", options
        if channel.register?
          add_action :general, :register, "#{channel.user}:#{channel.password}@#{channel.config['host_and_port']}/#{channel.application_id}"
        end
      end
    rescue Exception => ex
      puts ex
    end

    def delete_channel(channel_id)
      Asterisk::Conf.change SipConf do
        delete "verboice_#{channel_id}"
      end
    end
  end
end
