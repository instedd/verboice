# -*- mode: ruby -*-
# vi: set ft=ruby :

# Vagrantfile API/syntax version. Don't touch unless you know what you're doing!
VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  config.vm.box = "precise64"
  config.vm.box_url = "http://files.vagrantup.com/precise64.box"

  config.vm.provision :shell, :privileged => false, :inline => <<-SH
    # Add Erlang Solution's repository sources
    wget --no-check-certificate https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
    sudo dpkg -i erlang-solutions_1.0_all.deb

    # Install required packages
    sudo apt-get update
    export DEBIAN_FRONTEND=noninteractive
    sudo -E apt-get -y install ruby1.9.3 apache2 asterisk erlang erlang-dev mercurial git \
      libxml2-dev libxslt1-dev mysql-server libmysqlclient-dev lame sox libsox-fmt-mp3 nodejs \
      libcurl4-openssl-dev apache2-threaded-dev libapr1-dev libaprutil1-dev libyaml-dev postfix festival

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
    hg clone /vagrant verboice
    cd verboice
    bundle install --deployment --path .bundle --without "development test"
    bundle exec rake db:setup RAILS_ENV=production
    bundle exec rake assets:precompile
    make -C broker deps
    echo "Verboice::Application.config.action_mailer.delivery_method = :sendmail" > config/initializers/sendmail.rb
    script/update_erl_config broker/verboice.config verboice db_name verboice
    script/update_erl_config broker/verboice.config verboice asterisk_config_dir /etc/asterisk
    script/update_erl_config broker/verboice.config verboice asterisk_sounds_dir /usr/share/asterisk/sounds
    script/update_erl_config broker/verboice.config verboice base_url "http://192.168.33.10"
    script/update_erl_config broker/verboice.config verboice crypt_secret super_secret
    script/update_yml_config config/verboice.yml default_url_options host 192.168.33.10
    echo "RAILS_ENV=production" > .env
    sudo -E bundle exec foreman export upstart /etc/init -a verboice -u `whoami` --concurrency="broker=1,delayed=1"

    # Setup asterisk
    sudo rm -rf /etc/asterisk/*
    sudo cp etc/asterisk/* /etc/asterisk/
    sudo touch /etc/asterisk/sip_verboice_registrations.conf /etc/asterisk/sip_verboice_channels.conf
    sudo chown `whoami` /etc/asterisk/sip_verboice_*
    sudo mkdir /usr/share/asterisk/sounds/verboice
    sudo chown `whoami` /usr/share/asterisk/sounds/verboice
    sudo /etc/init.d/asterisk restart

    # Start verboice services
    sudo start verboice
  SH

  # Create a forwarded port mapping which allows access to a specific port
  # within the machine from a port on the host machine. In the example below,
  # accessing "localhost:8080" will access port 80 on the guest machine.
  # config.vm.network :forwarded_port, guest: 80, host: 8080

  # Create a private network, which allows host-only access to the machine
  # using a specific IP.
  config.vm.network :private_network, ip: "192.168.33.10"

  # Create a public network, which generally matched to bridged network.
  # Bridged networks make the machine appear as another physical device on
  # your network.
  # config.vm.network :public_network

  # If true, then any SSH connections made will enable agent forwarding.
  # Default value: false
  # config.ssh.forward_agent = true

  # Share an additional folder to the guest VM. The first argument is
  # the path on the host to the actual folder. The second argument is
  # the path on the guest to mount the folder. And the optional third
  # argument is a set of non-required options.
  # config.vm.synced_folder "../data", "/vagrant_data"

  # Provider-specific configuration so you can fine-tune various
  # backing providers for Vagrant. These expose provider-specific options.
  # Example for VirtualBox:
  #
  # config.vm.provider :virtualbox do |vb|
  #   # Don't boot with headless mode
  #   vb.gui = true
  #
  #   # Use VBoxManage to customize the VM. For example to change memory:
  #   vb.customize ["modifyvm", :id, "--memory", "1024"]
  # end
  #
end
