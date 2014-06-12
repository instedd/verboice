namespace :sanity_check do

  desc "Run a series of checks to cover for the most typical installation problems"
  task :run => :environment do
    puts BrokerClient.sanity_check()
  end

end
