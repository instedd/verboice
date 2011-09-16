module Asterisk
  class Broker < BaseBroker
    ConfigDir = Rails.configuration.asterisk_configuration[:config_dir]
    SipConf = "#{ConfigDir}/sip.conf"

    def call(session)
      check_asterisk_available!

      address = send "#{session.channel.kind}_address", session.channel, session.address

      result = $asterisk_client.originate({
        :channel => address,
        :application => 'AGI',
        :data => "agi://localhost:#{Asterisk::CallManager::Port},#{session.id}",
        :async => true,
        :actionid => session.id
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

    def generic_address(channel, address)
      index = channel.servers.find_index{|x| x.direction == 'outbound' || x.direction == 'both'}
      "SIP/verboice_#{channel.id}-#{index}/#{address}"
    end

    def reload!
      $asterisk_client.command :command => 'sip reload'
    end

    def create_sip2sip_channel(channel)
      section = "verboice_#{channel.id}"

      Asterisk::Conf.change SipConf do
        add section, :template => '!',
          :type => :peer,
          :canreinvite => :no,
          :nat => :yes,
          :qualify => :yes,
          :domain => 'sip2sip.info',
          :fromdomain => 'sip2sip.info',
          :outboundproxy => 'proxy.sipthor.net',
          :fromuser => channel.username,
          :defaultuser => channel.username,
          :secret => channel.password,
          :insecure => :invite,
          :context => :verboice

        ['sip2sip.info', '81.23.228.129', '81.23.228.150', '85.17.186.7'].each_with_index do |host, i|
          add "#{section}-#{i}", :template => section, :host => host
        end

        add_action :general, :register, "#{channel.username}:#{channel.password}@sip2sip.info/#{channel.id}"
      end
    end

    def delete_sip2sip_channel(channel)
      section = "verboice_#{channel.id}"

      Asterisk::Conf.change SipConf do
        remove section

        4.times { |i| remove "#{section}-#{i}" }

        remove_action :general, :register, "#{channel.username}:#{channel.password}@sip2sip.info/#{channel.id}"
      end
    end

    def create_callcentric_channel(channel)
      section = "verboice_#{channel.id}"

      Asterisk::Conf.change SipConf do
        add section,
          :type => :friend,
          :nat => :yes,
          :host => 'callcentric.com',
          :fromdomain => 'callcentric.com',
          :fromuser => channel.username,
          :defaultuser => channel.username,
          :secret => channel.password,
          :insecure => 'port,invite',
          :context => :verboice

        add_action :general, :register, "#{channel.username}:#{channel.password}@callcentric.com/#{channel.id}"
      end
    end

    def delete_callcentric_channel(channel)
      section = "verboice_#{channel.id}"

      Asterisk::Conf.change SipConf do
        remove section
        remove_action :general, :register, "#{channel.username}:#{channel.password}@callcentric.com/#{channel.id}"
      end
    end

    def create_generic_channel(channel)
      section = "verboice_#{channel.id}"

      Asterisk::Conf.change SipConf do
        add section, :template => '!',
          :type => :peer,
          :canreinvite => :no,
          :nat => :yes,
          :qualify => :yes,
          :fromuser => channel.username,
          :defaultuser => channel.username,
          :secret => channel.password,
          :insecure => :yes,
          :context => :verboice

        channel.servers.each_with_index do |server, i|
          add "#{section}-#{i}",
            :template => section,
            :host => server.host,
            :domain => server.host,
            :fromdomain => server.host,
            :type => (server.direction == 'inbound' ? 'user' : (server.direction == 'outbound' ? 'peer' : 'friend'))

          add_action :general, :register, "#{channel.username}:#{channel.password}@#{server.host}/#{channel.id}" if server.register?
        end
      end
    end

    def delete_generic_channel(channel)
      section = "verboice_#{channel.id}"

      Asterisk::Conf.change SipConf do
        remove section

        channel.servers.each_with_index do |server, i|
          remove "#{section}-#{i}"

          remove_action :general, :register, "#{channel.username}:#{channel.password}@#{server.host}/#{channel.id}" if server.register?
        end
      end
    end

    def check_asterisk_available!
      raise "Asterisk is not available" if $asterisk_client.nil? || $asterisk_client.error?
    end
  end
end
