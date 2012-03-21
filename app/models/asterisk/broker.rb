module Asterisk
  class Broker < BaseBroker
    ConfigDir = Rails.configuration.asterisk_configuration[:config_dir]
    SipConf = "#{ConfigDir}/sip.conf"

    def call(session)
      check_asterisk_available!

      address = get_dial_address session.channel, session.address

      result = $asterisk_client.originate({
        :channel => address,
        :application => 'AGI',
        :data => "agi://localhost:#{Asterisk::CallManager::Port},#{session.id}",
        :async => true,
        :actionid => session.id
      })

      result[:response] == 'Error' ? raise(result[:message]) : nil
    end

    def restart(session)
      $asterisk_client.redirect({
        :channel => session.pbx['channel'],
        :context => 'verboice-restart',
        :exten => session.id,
        :priority => 1
      })
    end

    def create_channel(channel)
      check_asterisk_available!
      regenerate_config
      reload!
    end

    def delete_channel(channel)
      check_asterisk_available!
      regenerate_config :delete => channel
      reload!
    end

    def get_dial_address(channel, address)
      send "#{channel.kind}_address", channel, address
    end

    private

    def sip_address(channel, address)
      index = channel.servers.find_index{|x| x.direction == 'outbound' || x.direction == 'both'}
      "SIP/verboice_#{channel.id}-#{index}/#{address}"
    end

    def custom_address(channel, address)
      channel.dial_string.gsub '{number}', address
    end

    def reload!
      $asterisk_client.command :command => 'sip reload'
    end

    def regenerate_config options = {}
      File.open("#{ConfigDir}/sip_verboice_registrations.conf", 'w') do |f_reg|
        File.open("#{ConfigDir}/sip_verboice_channels.conf", 'w') do |f_channels|
          Channel.where(:kind => 'sip').each do |channel|
            next if channel == options[:delete]
            section = "verboice_#{channel.id}"

            f_channels.puts "[#{section}](!)"
            f_channels.puts "type=peer"
            f_channels.puts "canreinvite=no"
            f_channels.puts "nat=yes"
            f_channels.puts "qualify=yes"
            f_channels.puts "fromuser=#{channel.username}"
            f_channels.puts "defaultuser=#{channel.username}"
            f_channels.puts "secret=#{channel.password}"
            f_channels.puts "insecure=yes"
            f_channels.puts "context=verboice"
            f_channels.puts

            channel.servers.each_with_index do |server, i|
              f_channels.puts "[#{section}-#{i}](#{section})"
              f_channels.puts "host=#{server.host}"
              f_channels.puts "domain=#{server.host}"
              f_channels.puts "fromdomain=#{server.host}"
              f_channels.puts "type=#{(server.direction == 'inbound' ? 'user' : (server.direction == 'outbound' ? 'peer' : 'friend'))}"

              f_reg.puts "register => #{channel.username}:#{channel.password}@#{server.host}/#{channel.id}" if server.register?
            end
          end
        end
      end
    end

    def pbx_available?
      $asterisk_client && !$asterisk_client.error?
    end

    def check_asterisk_available!
      raise PbxUnavailableException.new("Asterisk is not available") unless pbx_available?
    end
  end
end
