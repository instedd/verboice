# -*- mode: ruby -*-
# vi: set ft=ruby :

# Vagrantfile API/syntax version. Don't touch unless you know what you're doing!
VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  config.vm.box = "precise64"
  config.vm.box_url = "http://files.vagrantup.com/precise64.box"
  config.vm.hostname = "verboice.local"
  config.vm.network :private_network, type: :dhcp

  config.vm.provider :virtualbox do |vb|
    vb.customize ["modifyvm", :id, "--memory", "1024"]
  end

  config.vm.provision :shell, :privileged => false, :inline => <<-SH
    # Add Erlang Solution's repository sources
    wget --no-check-certificate https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
    sudo dpkg -i erlang-solutions_1.0_all.deb

    # Install required packages
    sudo apt-get update
    export DEBIAN_FRONTEND=noninteractive
    sudo -E apt-get -y install ruby1.9.3 apache2 asterisk erlang erlang-dev mercurial git \
      libxml2-dev libxslt1-dev libzmq-dev mysql-server libmysqlclient-dev sox libsox-fmt-mp3 nodejs \
      libcurl4-openssl-dev apache2-threaded-dev libapr1-dev libaprutil1-dev libyaml-dev postfix festival curl \
      openjdk-7-jre-headless avahi-daemon

    # Install ElasticSearch
    wget -q https://download.elasticsearch.org/elasticsearch/elasticsearch/elasticsearch-1.1.0.deb
    sudo dpkg -i elasticsearch-1.1.0.deb
    sudo service elasticsearch restart
    update-rc.d elasticsearch defaults

    # Install bundler
    sudo gem install bundler --no-ri --no-rdoc

    # Install passenger
    sudo gem install passenger --no-ri --no-rdoc
    sudo passenger-install-apache2-module -a
    sudo sh -c 'passenger-install-apache2-module --snippet > /etc/apache2/mods-available/passenger.load'
    sudo a2enmod passenger
    sudo sh -c 'echo "<VirtualHost *:80>
  DocumentRoot `pwd`/verboice/public
  PassengerSpawnMethod conservative
</VirtualHost>" > /etc/apache2/sites-enabled/000-default'
    sudo service apache2 restart

    # Setup rails application
    git clone /vagrant verboice
    cd verboice
    bundle install --deployment --path .bundle --without "development test"
    bundle exec rake db:setup RAILS_ENV=production
    bundle exec rake assets:precompile
    make -C broker deps
    echo "Verboice::Application.config.action_mailer.delivery_method = :sendmail" > config/initializers/sendmail.rb
    script/update_erl_config broker/verboice.config verboice db_name verboice
    script/update_erl_config broker/verboice.config verboice asterisk_config_dir /etc/asterisk
    script/update_erl_config broker/verboice.config verboice asterisk_sounds_dir /usr/share/asterisk/sounds
    script/update_erl_config broker/verboice.config verboice base_url "http://verboice.local"
    script/update_erl_config broker/verboice.config verboice crypt_secret super_secret
    script/update_yml_config config/verboice.yml default_url_options host verboice.local
    echo "RAILS_ENV=production" > .env
    sudo -E bundle exec foreman export upstart /etc/init -a verboice -u `whoami` --concurrency="broker=1,delayed=1"

    # Setup asterisk
    sudo rm -rf /etc/asterisk/*
    sudo cp etc/asterisk/* /etc/asterisk/
    sudo touch /etc/asterisk/sip_verboice_registrations.conf /etc/asterisk/sip_verboice_channels.conf
    sudo chown `whoami` /etc/asterisk/sip_verboice_*
    sudo mkdir -p /usr/share/asterisk/sounds/verboice
    sudo chown `whoami` /usr/share/asterisk/sounds/verboice
    sudo /etc/init.d/asterisk restart

    # Start verboice services
    sudo start verboice
  SH
end
