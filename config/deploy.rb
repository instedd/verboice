# Copyright (C) 2010-2012, InSTEDD
#
# This file is part of Verboice.
#
# Verboice is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Verboice is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Verboice.  If not, see <http://www.gnu.org/licenses/>.

require 'bundler/capistrano'
require 'rvm/capistrano'

set :rvm_ruby_string, '1.9.3'
set :rvm_type, :system

set :application, "verboice"
set :repository,  "https://bitbucket.org/instedd/verboice"
set :scm, :git
set :deploy_via, :remote_cache
set :user, 'ubuntu'

default_environment['TERM'] = ENV['TERM']

# role :web, "your web-server here"                          # Your HTTP server, Apache/etc
# role :app, "your app-server here"                          # This may be the same as your `Web` server
# role :db,  "your primary db-server here", :primary => true # This is where Rails migrations will run
# role :db,  "your slave db-server here"

namespace :deploy do
  task :start do ; end
  task :stop do ; end
  task :restart, :roles => :app, :except => { :no_release => true } do
    run "#{try_sudo} touch #{File.join(current_path,'tmp','restart.txt')}"
  end

  task :prepare_broker, :roles => :app do
    run "test -f #{shared_path}/verboice.config || cp #{release_path}/broker/verboice.config #{shared_path}"
    run "ln -nfs #{shared_path}/verboice.config #{release_path}/broker/verboice.config"

    run "test -d #{shared_path}/log/broker || mkdir #{shared_path}/log/broker"
    run "ln -nfs #{shared_path}/log/broker #{release_path}/broker/log"
  end

  task :compile_broker, :roles => :app do
    run "make -C #{release_path}/broker"
  end

  task :symlink_configs, :roles => :app do
    %W(credentials verboice newrelic oauth nuntium poirot guisso).each do |file|
      run "ln -nfs #{shared_path}/#{file}.yml #{release_path}/config/"
    end
  end

  task :symlink_data, :roles => :app do
    run "ln -nfs #{shared_path}/data #{release_path}/"
  end

  task :generate_version, :roles => :app do
    run "cd #{release_path} && git describe --always > #{release_path}/VERSION"
  end
end

namespace :foreman do
  desc 'Export the Procfile to Ubuntu upstart scripts'
  task :export, :roles => :app do
    run "echo -e \"PATH=$PATH\\nGEM_HOME=$GEM_HOME\\nGEM_PATH=$GEM_PATH\\nRAILS_ENV=production\" >  #{current_path}/.env"
    run "cd #{current_path} && rvmsudo bundle exec foreman export upstart /etc/init -f #{current_path}/Procfile -a #{application} -u #{user} --concurrency=\"broker=1,delayed=1\""
  end

  desc "Start the application services"
  task :start, :roles => :app do
    sudo "start #{application}"
  end

  desc "Stop the application services"
  task :stop, :roles => :app do
    sudo "stop #{application}"
  end

  desc "Restart the application services"
  task :restart, :roles => :app do
    run "sudo start #{application} || sudo restart #{application}"
  end
end


before "deploy:start", "deploy:migrate"
before "deploy:restart", "deploy:migrate"
after "deploy:update_code", "deploy:generate_version"
after "deploy:update_code", "deploy:symlink_configs"
after "deploy:update_code", "deploy:symlink_data"
after "deploy:update_code", "deploy:prepare_broker"
after "deploy:update_code", "deploy:compile_broker"

after "deploy:update", "foreman:export"    # Export foreman scripts
after "deploy:restart", "foreman:restart"   # Restart application scripts
