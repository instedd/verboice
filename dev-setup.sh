#!/bin/sh -e
docker compose build

docker compose run --rm broker sh -c './rebar get-deps && ./rebar compile'

docker compose run --rm web bundle install
docker compose run --rm web bundle exec rake db:setup
docker compose run --rm web bundle exec rake db:test:prepare
docker compose run --rm -v `pwd`:/app asterisk /bin/sh -c 'cp /app/etc/asterisk/* /etc/asterisk'
