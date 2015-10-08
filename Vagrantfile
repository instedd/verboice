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

  config.vm.synced_folder "backups/", "/home/vagrant/backups"

  config.vm.provision :shell do |s|
    s.privileged = false
    s.args = [ENV['WITH_ELASTICSEARCH'] || "", ENV['REVISION'] || ""]
    s.inline = <<-SH

    export DEBIAN_FRONTEND=noninteractive

    # Add Erlang Solution's repository sources
    wget --no-check-certificate https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
    sudo dpkg -i erlang-solutions_1.0_all.deb

    # Add repository for libzmq3
    sudo apt-get update
    sudo -E apt-get install -y python-software-properties
    sudo -E add-apt-repository -y ppa:chris-lea/zeromq

    # Install required packages
    sudo apt-get update
    sudo -E apt-get -y install ruby1.9.3 apache2 asterisk mercurial git \
      libxml2-dev libxslt1-dev libzmq3-dbg libzmq3-dev libzmq3 mysql-server libmysqlclient-dev sox libsox-fmt-mp3 nodejs \
      libcurl4-openssl-dev apache2-threaded-dev libapr1-dev libaprutil1-dev libyaml-dev postfix festival curl \
      openjdk-7-jre-headless avahi-daemon
    sudo -E apt-get -y install erlang-ic=1:17.5.3 erlang-diameter=1:17.5.3 erlang-eldap=1:17.5.3 erlang-base=1:17.5.3 \
      erlang-crypto=1:17.5.3 erlang-runtime-tools=1:17.5.3 erlang-mnesia=1:17.5.3 erlang-ssl=1:17.5.3 \
      erlang-syntax-tools=1:17.5.3 erlang-asn1=1:17.5.3 erlang-public-key=1:17.5.3 erlang=1:17.5.3 erlang-dev=1:17.5.3 \
      erlang-appmon=1:17.5.3 erlang-common-test=1:17.5.3 erlang-corba=1:17.5.3 erlang-debugger=1:17.5.3 \
      erlang-dialyzer=1:17.5.3 erlang-edoc=1:17.5.3 erlang-erl-docgen=1:17.5.3 erlang-et=1:17.5.3 erlang-eunit=1:17.5.3 \
      erlang-gs=1:17.5.3 erlang-inets=1:17.5.3 erlang-inviso=1:17.5.3 erlang-megaco=1:17.5.3 erlang-observer=1:17.5.3 \
      erlang-odbc=1:17.5.3 erlang-os-mon=1:17.5.3 erlang-parsetools=1:17.5.3 erlang-percept=1:17.5.3 erlang-pman=1:17.5.3 \
      erlang-reltool=1:17.5.3 erlang-snmp=1:17.5.3 erlang-ssh=1:17.5.3 erlang-test-server=1:17.5.3 erlang-toolbar=1:17.5.3 \
      erlang-tools=1:17.5.3 erlang-tv=1:17.5.3 erlang-typer=1:17.5.3 erlang-webtool=1:17.5.3 erlang-wx=1:17.5.3 erlang-xmerl=1:17.5.3

    # Configure mysql
    sudo sh -c 'echo "[mysqld]
max_allowed_packet = 256M" > /etc/mysql/conf.d/mysqld.cnf'
    sudo service mysql restart

    # Install ElasticSearch
    if [ "$1" == '1' ]; then
      wget -q https://download.elasticsearch.org/elasticsearch/elasticsearch/elasticsearch-1.1.0.deb
      sudo dpkg -i elasticsearch-1.1.0.deb
      sudo service elasticsearch restart
      update-rc.d elasticsearch defaults
    fi

    # Install bundler
    sudo gem install bundler --no-ri --no-rdoc

    # Install passenger
    sudo gem install passenger --no-ri --no-rdoc
    sudo passenger-install-apache2-module -a
    sudo sh -c 'passenger-install-apache2-module --snippet > /etc/apache2/mods-available/passenger.load'
    sudo a2enmod passenger

    # Configure apache website for Verboice
    sudo sh -c 'echo "<VirtualHost *:80>
  DocumentRoot `pwd`/verboice/public
  PassengerSpawnMethod conservative
</VirtualHost>" > /etc/apache2/sites-enabled/000-default'

    # Configure apache website for admin UI
    sudo sh -c 'echo "NameVirtualHost *:8080
Listen 8080"' >> /etc/apache2/ports.conf
    sudo sh -c 'echo "<VirtualHost *:8080>
  DocumentRoot `pwd`/verboice/admin/public
  PassengerSpawnMethod conservative
  SetEnv RAILS_ENV production
  SetEnv VERBOICE_BACKUPS /home/vagrant/backups
</VirtualHost>" > /etc/apache2/sites-enabled/001-admin'

    sudo service apache2 restart

    # Create logs folder
    sudo mkdir -p /var/log/verboice
    sudo chown `whoami` /var/log/verboice

    # Setup rails application and broker
    git clone /vagrant verboice
    cd verboice
    if [ "$2" != '' ]; then
      git checkout $2;
      echo $2 > VERSION;
    fi

    bundle install --deployment --path .bundle --without "development test"
    bundle exec rake db:setup RAILS_ENV=production
    bundle exec rake assets:precompile
    make -C broker deps

    # Configuration for not using elasticsearch
    if [ "$1" != '1' ]; then
      cp broker/verboice.config.no-es broker/verboice.config
      script/update_yml_config config/verboice.yml poirot_elasticsearch_url null
    fi

    # Configuration changes
    echo "Verboice::Application.config.action_mailer.delivery_method = :sendmail" > config/initializers/sendmail.rb
    script/update_erl_config broker/verboice.config verboice db_name verboice
    script/update_erl_config broker/verboice.config verboice asterisk_config_dir /etc/asterisk
    script/update_erl_config broker/verboice.config verboice asterisk_sounds_dir /usr/share/asterisk/sounds
    script/update_erl_config broker/verboice.config verboice base_url "http://verboice.local"
    script/update_erl_config broker/verboice.config verboice crypt_secret super_secret
    script/update_yml_config config/verboice.yml default_url_options host verboice.local
    echo "RAILS_ENV=production" > .env
    echo "HOME=$HOME" >> .env
    sudo -E bundle exec foreman export upstart /etc/init -a verboice -u `whoami` --concurrency="broker=1,delayed=1"

    # Setup admin interface
    cd ~/verboice/admin
    bundle install --deployment --path .bundle --without "development test"
    mkdir -p ~/verboice/data/call_logs
    mkdir -p ~/backups

    # Schedule regular backups
    sudo cp /home/vagrant/verboice/admin/etc/backup-verboice /etc/cron.weekly/
    sudo chmod a+x /etc/cron.weekly/backup-verboice

    # Setup asterisk
    cd ~/verboice
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
end
