#!/bin/sh
NC='\033[0m'
GREEN='\033[0;32m'

echo "----==== Running web tests ====----"
WEB_TESTS="$(docker-compose run --rm web rspec)"

if [ $? -eq 0 ]; then
  echo "${GREEN}OK${NC}";
else
  echo "${WEB_TESTS}"
fi

echo "----==== Running broker tests ====----"
BROKER_TESTS="$(docker-compose run --rm broker make eunit)"

if [ $? -eq 0 ]; then
  echo "${GREEN}OK${NC}";
else
  echo "${BROKER_TESTS}"
fi
