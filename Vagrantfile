# -*- mode: ruby -*-
# vi: set ft=ruby :

# Vagrantfile API/syntax version. Don't touch unless you know what you're doing!
VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  config.vm.box = "ubuntu/trusty64"
  config.vm.hostname = "verboice.local"

  config.vm.network :forwarded_port, guest: "80", host: "8085", host_ip: "127.0.0.1"
  config.vm.network :public_network, ip: '192.168.1.15'

  config.vm.provider :virtualbox do |vb|
    vb.customize ["modifyvm", :id, "--memory", "4096"]
  end

  config.vm.provision :shell do |s|
    s.privileged = false
    s.args = [ENV['REVISION'] || "3.1"]
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
    sudo -E apt-get -y install ruby1.9.3 apache2 mercurial git \
      libxml2-dev libxslt1-dev libzmq3-dbg libzmq3-dev libzmq3 mysql-server libmysqlclient-dev sox libsox-fmt-mp3 nodejs \
      libcurl4-openssl-dev apache2-threaded-dev libapr1-dev libaprutil1-dev libyaml-dev postfix festival curl \
      openjdk-7-jre-headless avahi-daemon \
      build-essential pkg-config libncurses5-dev uuid-dev libjansson-dev libsqlite3-dev
    sudo -E apt-get -y install erlang-ic=1:17.5.3 erlang-diameter=1:17.5.3 erlang-eldap=1:17.5.3 erlang-base=1:17.5.3 \
      erlang-crypto=1:17.5.3 erlang-runtime-tools=1:17.5.3 erlang-mnesia=1:17.5.3 erlang-ssl=1:17.5.3 \
      erlang-syntax-tools=1:17.5.3 erlang-asn1=1:17.5.3 erlang-public-key=1:17.5.3 erlang=1:17.5.3 erlang-dev=1:17.5.3 \
      erlang-appmon=1:17.5.3 erlang-common-test=1:17.5.3 erlang-corba=1:17.5.3 erlang-debugger=1:17.5.3 \
      erlang-dialyzer=1:17.5.3 erlang-edoc=1:17.5.3 erlang-erl-docgen=1:17.5.3 erlang-et=1:17.5.3 erlang-eunit=1:17.5.3 \
      erlang-gs=1:17.5.3 erlang-inets=1:17.5.3 erlang-inviso=1:17.5.3 erlang-megaco=1:17.5.3 erlang-observer=1:17.5.3 \
      erlang-odbc=1:17.5.3 erlang-os-mon=1:17.5.3 erlang-parsetools=1:17.5.3 erlang-percept=1:17.5.3 erlang-pman=1:17.5.3 \
      erlang-reltool=1:17.5.3 erlang-snmp=1:17.5.3 erlang-ssh=1:17.5.3 erlang-test-server=1:17.5.3 erlang-toolbar=1:17.5.3 \
      erlang-tools=1:17.5.3 erlang-tv=1:17.5.3 erlang-typer=1:17.5.3 erlang-webtool=1:17.5.3 erlang-wx=1:17.5.3 erlang-xmerl=1:17.5.3

    # Install PJproject for PJSIP
    PJPROJECT_VERSION=2.5.5
    PJPROJECT_URL=http://www.pjsip.org/release/${PJPROJECT_VERSION}/pjproject-${PJPROJECT_VERSION}.tar.bz2


    if [ ! -f /usr/lib/libpj.so ]; then
      if [ ! -d pjproject-${PJPROJECT_VERSION} ]; then
        curl -s $PJPROJECT_URL | tar -xj
      fi

      cd pjproject-${PJPROJECT_VERSION}
      ./configure --prefix=/usr \
                  --enable-shared \
                  --disable-sound \
                  --disable-resample \
                  --disable-video \
                  --disable-opencore-amr \
                  CFLAGS='-O2 -DNDEBUG'

      make dep
      make
      sudo make install
      sudo ldconfig
      cd ..
    fi

    # Install Asterisk
    ASTERISK_VERSION=13.13
    ASTERISK_URL=http://downloads.asterisk.org/pub/telephony/certified-asterisk/asterisk-certified-${ASTERISK_VERSION}-current.tar.gz

    if [ ! -f /etc/init.d/asterisk ]; then
      if [ ! -d asterisk-${ASTERISK_VERSION} ]; then
        mkdir asterisk-${ASTERISK_VERSION}
        curl -s $ASTERISK_URL | tar -xvz --strip-components=1 -C asterisk-${ASTERISK_VERSION}
      fi

      cd asterisk-${ASTERISK_VERSION}
      ./configure
      make menuselect.makeopts
      make
      sudo make install
      sudo make config

      cd ..
    fi

    # Configure mysql
    sudo sh -c 'echo "[mysqld]
max_allowed_packet = 256M" > /etc/mysql/conf.d/mysqld.cnf'
    sudo service mysql restart

    # Install bundler
    sudo gem install bundler --no-ri --no-rdoc

    # Install passenger
    sudo gem install rack -v 1.6.4 --no-ri --no-rdoc
    sudo gem install passenger -v 5.0.23 --no-ri --no-rdoc
    sudo passenger-install-apache2-module -a
    sudo sh -c 'passenger-install-apache2-module --snippet > /etc/apache2/mods-available/passenger.load'
    sudo a2enmod passenger

    # Configure apache website for Verboice
    sudo sh -c 'echo "<VirtualHost *:80>
  DocumentRoot `pwd`/verboice/public
  PassengerSpawnMethod conservative
  <Directory `pwd`/verboice/public>
    Allow from all
    Options -MultiViews
    Require all granted
  </Directory>
</VirtualHost>" > /etc/apache2/sites-available/000-default.conf'

    # Configure apache website for admin UI
    sudo sh -c 'echo "NameVirtualHost *:8080
Listen 8080"' >> /etc/apache2/ports.conf
    sudo sh -c 'echo "<VirtualHost *:8080>
  DocumentRoot `pwd`/verboice/admin/public
  PassengerSpawnMethod conservative
  SetEnv RAILS_ENV production
  SetEnv VERBOICE_BACKUPS /home/vagrant/backups
  <Directory `pwd`/verboice/admin/public>
    Allow from all
    Options -MultiViews
    Require all granted
  </Directory>
</VirtualHost>" > /etc/apache2/sites-enabled/001-admin.conf'

    sudo service apache2 restart

    # Create logs folder
    sudo mkdir -p /var/log/verboice
    sudo chown `whoami` /var/log/verboice

    # Setup rails application and broker
    git clone /vagrant verboice
    cd verboice
    if [ "$1" != '' ]; then
      git checkout $1;
      echo $1 > VERSION;
    fi

    bundle install --deployment --path .bundle --without "development test"
    bundle exec rake db:setup RAILS_ENV=production
    bundle exec rake assets:precompile
    make -C broker deps

    # Configuration for not using elasticsearch
    cp broker/verboice.config.no-es broker/verboice.config

    # Configuration changes
    echo "Verboice::Application.config.action_mailer.delivery_method = :sendmail" > config/initializers/sendmail.rb
    script/update_erl_config broker/verboice.config verboice db_name verboice
    script/update_erl_config broker/verboice.config verboice asterisk_config_dir /etc/asterisk
    script/update_erl_config broker/verboice.config verboice asterisk_sounds_dir /var/lib/asterisk/sounds
    script/update_erl_config broker/verboice.config verboice base_url "http://verboice.local"
    script/update_erl_config broker/verboice.config verboice crypt_secret super_secret
    script/update_yml_config config/verboice.yml default_url_options host verboice.local
    script/update_yml_config config/verboice.yml skip_account_confirmation true
    echo "RAILS_ENV=production" > .env
    echo "HOME=$HOME" >> .env
    sudo -E bundle exec foreman export upstart /etc/init -a verboice -u `whoami` -t etc/upstart --concurrency="broker=1,delayed=1"

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
    sudo touch /etc/asterisk/pjsip_verboice.conf
    sudo chown `whoami` /etc/asterisk/pjsip_verboice.conf
    sudo mkdir -p /var/lib/asterisk/sounds/verboice
    sudo chown `whoami` /var/lib/asterisk/sounds/verboice
    sudo /etc/init.d/asterisk restart

    # Start verboice services
    sudo start verboice
  SH
  end
end
