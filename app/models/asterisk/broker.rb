module Asterisk
  class Broker < BaseBroker
    ConfigDir = Rails.configuration.asterisk_configuration[:config_dir]
    SipConf = "#{ConfigDir}/sip.conf"

    attr_accessor :asterisk_client

    def call(queued_call)
      check_asterisk_available!

      queued_call.call_log.start

      address = send "#{queued_call.channel.kind}_address", queued_call.channel, queued_call.address

      result = asterisk_client.originate({
        :channel => address,
        :application => 'AGI',
        :data => "agi://localhost:#{Asterisk::CallManager::Port},#{queued_call.channel.id},#{queued_call.call_log.id}",
        :async => true,
        :actionid => queued_call.call_log.id
      })

      result[:response] == 'Error' ? raise(result[:message]) : nil
    end

    def create_channel(channel)
      check_asterisk_available!

      send "create_#{channel.kind}_channel", channel

      reload!
    end

    def delete_channel(channel)
      check_asterisk_available!

      send "delete_#{channel.kind}_channel", channel

      reload!
    end

    private

    def sip2sip_address(channel, address)
      "SIP/verboice_#{channel.id}-0/#{address}"
    end

    def callcentric_address(channel, address)
      "SIP/verboice_#{channel.id}/#{address}"
    end


    def reload!
      asterisk_client.command :command => 'sip reload'
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

    def create_callcentric_channel(channel)
      section = "verboice_#{channel.id}"
      user = channel.config['username']
      password = channel.config['password']

      Asterisk::Conf.change SipConf do
        add section,
          :type => :friend,
          :nat => :yes,
          :host => 'callcentric.com',
          :fromdomain => 'callcentric.com',
          :fromuser => user,
          :defaultuser => user,
          :secret => password,
          :insecure => 'port,invite',
          :context => :verboice

        add_action :general, :register, "#{user}:#{password}@callcentric.com/#{channel.id}"
      end
    end

    def delete_callcentric_channel(channel)
      section = "verboice_#{channel.id}"
      user = channel.config['username']
      password = channel.config['password']

      Asterisk::Conf.change SipConf do
        remove section
        remove_action :general, :register, "#{user}:#{password}@callcentric.com/#{channel.id}"
      end
    end

    def check_asterisk_available!
      raise "Asterisk is not available" if asterisk_client.error?
    end
  end
end
