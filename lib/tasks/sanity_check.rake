require "#{Rails.root}/app/helpers/application_helper"

include ApplicationHelper

namespace :sanity_check do

  desc "Run a series of checks to cover for the most typical installation problems"

  def sanity_check_rails()
    if nuntium_configured?
      begin
        Pigeon::NuntiumChannel.all
        {:message=> "ok", :status=> :ok, :name=> "Nuntium Credentials"}
      rescue Pigeon::NuntiumException => e
        if e.message == "401 Unauthorized"
          {:message=> "Invalid Credentials", :status=> :error, :name=> "Nuntium Credentials"}
        end
      end
    else
      {:message=> "Not Configured", :status=> :ok, :name=> "Nuntium Credentials"}
    end
  end

  task :run => :environment do
    puts BrokerClient.sanity_check()
    puts sanity_check_rails()
  end

end
