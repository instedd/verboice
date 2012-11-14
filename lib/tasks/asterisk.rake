namespace :asterisk do
  desc "Copy asterisk .conf and .ael files to asterisk configuration directory"
  task :setup => :environment do
    require 'fileutils'

    Dir["#{Rails.root}/etc/asterisk/*"].each do |file|
      puts "cp #{file} #{Asterisk::ConfigDir}/"
      FileUtils.cp file, "#{Asterisk::ConfigDir}/"
    end
  end
end
