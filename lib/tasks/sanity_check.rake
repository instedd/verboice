require "#{Rails.root}/app/helpers/application_helper"

include ApplicationHelper

desc "Run a series of checks to cover for the most typical installation problems"
task :sanity_check => :environment do

  ok = {status: :ok}
  def skip(message); {status: :skip, message: message}; end

  def run_checks(checks)
    checks.each do |description, proc|
      print "#{description}..."
      begin
        result = proc.call
        case result[:status]
        when :ok
          puts " OK".green
        when :skip
          puts " SKIP (#{result[:message]})".yellow
        when :error
          puts " ERROR (#{result[:message]})".red
        end
      rescue Exception => ex
        puts " ERROR (#{ex.message})".red
      end
    end
  end

  broker_checks = nil
  rails_checks = {
    "Broker connectivity" => -> {
      if BrokerClient.ping == :ok
        broker_checks = {}
        BrokerClient.sanity_check_list.each_with_index do |description, i|
          broker_checks[description] = -> { BrokerClient.sanity_check(i + 1) }
        end
      end
      ok
    },
    "Nuntium credentials" => -> { nuntium_configured? ? (Pigeon::NuntiumChannel.all; ok) : skip("Not configured")}
  }

  run_checks(rails_checks)

  if broker_checks
    run_checks(broker_checks)
  else
    puts
    print "WARNING: ".red
    puts "There was an error while trying to connect with the broker. The broker tests are not run"
  end

end

class String
  def colorize(color_code)
    "\e[#{color_code}m#{self}\e[0m"
  end

  def red; colorize(31); end
  def green; colorize(32); end
  def yellow; colorize(33); end
end
