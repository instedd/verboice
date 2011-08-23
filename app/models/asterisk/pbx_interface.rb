module Asterisk
  class PbxInterface < MagicObjectProtocol::Server
    Port = Rails.configuration.verboice_configuration[:pbx_interface_port].to_i
    ConfigDir = Rails.configuration.asterisk_configuration[:config_dir]
    SipConf = "#{ConfigDir}/sip.conf"

    attr_accessor :pbx

    def try_call_from_queue channel_id
      channel = Channel.find channel_id
      if channel.can_call?
        queued_call = CallQueue.poll channel
        self.call(queued_call.address, queued_call.channel, queued_call.call_log) if queued_call
      end
    end
    
    def call(address, channel, call_log)
      call_log.start
      
      raise "PBX is not available" if pbx.error?

      address = send "#{channel.kind}_address", channel, address

      result = pbx.originate :channel => address,
        :application => 'AGI',
        :data => "agi://localhost:#{Asterisk::FastAGIServer::Port},#{channel.id},#{call_log.id}",
        :async => true,
        :actionid => call_log.id

      result[:response] == 'Error' ? raise(result[:message]) : nil
    end

    def sip2sip_address(channel, address)
      "SIP/verboice_#{channel.id}-0/#{address}"
    end

    def create_channel(channel_id)
      raise "PBX is not available" if pbx.error?

      channel = Channel.find channel_id
      send "create_#{channel.kind}_channel", channel

      reload!
    rescue Exception => ex
      puts "#{ex}, #{ex.backtrace}"
    end

    def delete_channel(channel_id)
      raise "PBX is not available" if pbx.error?

      channel = Channel.find_by_id channel_id
      send "delete_#{channel.kind}_channel", channel

      reload!
    end

    def reload!
      pbx.command :command => 'sip reload'
    end

    def create_sip2sip_channel(channel)
      section = "verboice_#{channel.id}"
      user = channel.config['username']
      password = channel.config['password']

      Asterisk::Conf.change SipConf do
        add section, :template => '!',
          :type => :peer,
          :canreinvite => :no,
          :nat => :yes,
          :qualify => :yes,
          :domain => 'sip2sip.info',
          :fromdomain => 'sip2sip.info',
          :outboundproxy => 'proxy.sipthor.net',
          :fromuser => user,
          :defaultuser => user,
          :secret => password,
          :insecure => :invite,
          :context => :verboice

        ['sip2sip.info', '81.23.228.129', '81.23.228.150', '85.17.186.7'].each_with_index do |host, i|
          add "#{section}-#{i}", :template => section, :host => host
        end

        add_action :general, :register, "#{user}:#{password}@sip2sip.info/#{channel.id}"
      end
    end

    def delete_sip2sip_channel(channel)
      section = "verboice_#{channel.id}"
      user = channel.config['username']
      password = channel.config['password']

      Asterisk::Conf.change SipConf do
        remove section

        4.times { |i| remove "#{section}-#{i}" }

        remove_action :general, :register, "#{user}:#{password}@sip2sip.info/#{channel.id}"
      end
    end
  end
end
