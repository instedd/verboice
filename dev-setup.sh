#!/bin/sh
docker-compose build

# The next two commands occasionally fail. If so, replace them with:
# docker-compose run --rm broker bash
#   ./rebar get-deps
#   ./rebar compile
docker-compose run --rm broker ./rebar get-deps
docker-compose run --rm broker ./rebar compile

docker-compose run --rm web bundle install
docker-compose run --rm web bundle exec rake db:setup
docker-compose run --rm web bundle exec rake db:test:prepare
docker-compose run --rm -v `pwd`:/app asterisk /bin/sh -c 'cp /app/etc/asterisk/* /etc/asterisk'
